/*
  Simple DirectMedia Layer
  Copyright (C) 2024 Katharine Chui <katharine.chui@gmail.com>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/

#include "../../SDL_internal.h"

#ifdef SDL_JOYSTICK_HIDAPI

#include "SDL_hidapihaptic_c.h"

#ifdef SDL_HAPTIC_HIDAPI_LG4FF

#include "SDL_thread.h"
#include "SDL_mutex.h"
#include "SDL_timer.h"

#include <math.h>

#define USB_VENDOR_ID_LOGITECH 0x046d
#define USB_DEVICE_ID_LOGITECH_G29_WHEEL 0xc24f
#define USB_DEVICE_ID_LOGITECH_G27_WHEEL 0xc29b
#define USB_DEVICE_ID_LOGITECH_G25_WHEEL 0xc299
#define USB_DEVICE_ID_LOGITECH_DFGT_WHEEL 0xc29a
#define USB_DEVICE_ID_LOGITECH_DFP_WHEEL 0xc298
#define USB_DEVICE_ID_LOGITECH_WHEEL 0xc294

static Uint32 supported_device_ids[] = {
    USB_DEVICE_ID_LOGITECH_G29_WHEEL,
    USB_DEVICE_ID_LOGITECH_G27_WHEEL,
    USB_DEVICE_ID_LOGITECH_G25_WHEEL,
    USB_DEVICE_ID_LOGITECH_DFGT_WHEEL,
    USB_DEVICE_ID_LOGITECH_DFP_WHEEL,
    USB_DEVICE_ID_LOGITECH_WHEEL
};

/*
  effect rendering and command sending are ported from https://github.com/berarma/new-lg4ff
*/

#define LG4FF_MAX_EFFECTS 16

#define FF_EFFECT_STARTED 0
#define FF_EFFECT_ALLSET 1
#define FF_EFFECT_PLAYING 2
#define FF_EFFECT_UPDATING 3

struct lg4ff_effect_state{
	SDL_HapticEffect effect;
	Uint64 start_at;
	Uint64 play_at;
	Uint64 stop_at;
	Uint32 flags;
	Uint64 time_playing;
	Uint64 updated_at;
	Uint32 phase;
	Uint32 phase_adj;
	Uint32 count;

	// unused?
	#if 0
	Uint32 cmd;
	Uint64 cmd_start_time;
	Uint32 cmd_start_count;
	#endif

	double direction_gain;
	Sint32 slope;
};

struct lg4ff_effect_parameters{
	Sint32 level;
	Sint32 d1;
	Sint32 d2;
	Sint32 k1;
	Sint32 k2;
	Uint32 clip;
};

struct lg4ff_slot{
	Sint32 id;
	struct lg4ff_effect_parameters parameters;
	Uint8 current_cmd[7];
	Uint32 cmd_op;
	bool is_updated;
	Sint32 effect_type;
};

typedef struct lg4ff_device{
	Uint16 product_id;
	struct lg4ff_effect_state states[LG4FF_MAX_EFFECTS];
	struct lg4ff_slot slots[4];
	Sint32 effects_used;

	Sint32 gain;
	Sint32 app_gain;

	Sint32 spring_level;
	Sint32 damper_level;
	Sint32 friction_level;

	Sint32 peak_ffb_level;

	SDL_Joystick *hid_handle;

   SDL_bool stop_thread;
   SDL_Thread *thread;
   char *thread_name_buf[256];

   SDL_mutex *mutex;
} lg4ff_device;

Uint64 get_time_ms(){
   return SDL_GetTicks();
}

#define test_bit(bit, field) (*(field) & (1 << bit))
#define __set_bit(bit, field) {*(field) = *(field) | (1 << bit);}
#define __clear_bit(bit, field) {*(field) = *(field) & ~(1 << bit);}
#define sin_deg(in) (double)(sin((double)(in) * (double)M_PI / 180.0))

#define time_after_eq(a, b) (a >= b)
#define time_before(a, b) (a < b)
#define time_diff(a, b) (a - b)

#define STOP_EFFECT(state) ((state)->flags = 0)

#define CLAMP_VALUE_U16(x) ((Uint16)((x) > 0xffff ? 0xffff : (x)))
#define SCALE_VALUE_U16(x, bits) (CLAMP_VALUE_U16(x) >> (16 - bits))
#define CLAMP_VALUE_S16(x) ((Uint16)((x) <= -0x8000 ? -0x8000 : ((x) > 0x7fff ? 0x7fff : (x))))
#define TRANSLATE_FORCE(x) ((CLAMP_VALUE_S16(x) + 0x8000) >> 8)
#define SCALE_COEFF(x, bits) SCALE_VALUE_U16(abs(x) * 2, bits)

SDL_bool effect_is_periodic(Uint16 type)
{
   // XXX SDL ate square, wine is on 2.x and hence does not use square at all
   // TODO when porting to 3.0, handle SDL_HAPTIC_SQUARE
   return type == SDL_HAPTIC_SINE ||
      type == SDL_HAPTIC_TRIANGLE ||
      type == SDL_HAPTIC_SAWTOOTHUP ||
      type == SDL_HAPTIC_SAWTOOTHDOWN;
}

SDL_bool effect_is_condition(Uint16 type)
{
   return type == SDL_HAPTIC_SPRING ||
      type == SDL_HAPTIC_DAMPER ||
      type == SDL_HAPTIC_FRICTION;
}

Uint32 get_effect_replay_length(SDL_HapticEffect *effect)
{
   if (effect_is_periodic(effect->type)) {
      return effect->periodic.length;
   }

   if (effect_is_condition(effect->type)) {
      return effect->condition.length;
   }

   switch(effect->type) {
      case SDL_HAPTIC_CONSTANT:
         return effect->constant.length;
      case SDL_HAPTIC_RAMP:
         return effect->ramp.length;
   }

   SDL_assert(0);
   return 0;
}

Uint16 get_effect_replay_delay(SDL_HapticEffect *effect)
{
   if (effect_is_periodic(effect->type)) {
      return effect->periodic.delay;
   }

   if (effect_is_condition(effect->type)) {
      return effect->condition.delay;
   }

   switch(effect->type) {
      case SDL_HAPTIC_CONSTANT:
         return effect->constant.delay;
      case SDL_HAPTIC_RAMP:
         return effect->ramp.delay;
   }

   SDL_assert(0);
   return 0;
}

int lg4ff_play_effect(struct lg4ff_device *device, int effect_id, int value, bool log)
{
	struct lg4ff_effect_state *state;
	Uint64 now = get_time_ms();

	state = &device->states[effect_id];

	if (value > 0) {
		if (test_bit(FF_EFFECT_STARTED, &state->flags)) {
			STOP_EFFECT(state);
		} else {
			device->effects_used++;
		}
		__set_bit(FF_EFFECT_STARTED, &state->flags);
		state->start_at = now;
		state->count = value;
		if(log){
			STDOUT("--play--\n");
			log_effect(&state->effect);
		}
	} else {
		if (test_bit(FF_EFFECT_STARTED, &state->flags)) {
			STOP_EFFECT(state);
			device->effects_used--;
		}
	}

	return 0;
}

int lg4ff_upload_effect(struct lg4ff_device *device, struct ff_effect *effect, struct ff_effect *old, bool log)
{
	struct lg4ff_effect_state *state;
	Uint64 now = get_time_ms();

	if (effect->type == FF_PERIODIC && effect->u.periodic.period == 0) {
		return -EINVAL;
	}

	state = &device->states[effect->id];

	if (test_bit(FF_EFFECT_STARTED, &state->flags) && effect->type != state->effect.type) {
		return -EINVAL;
	}

	state->effect = *effect;

	if (test_bit(FF_EFFECT_STARTED, &state->flags)) {
		__set_bit(FF_EFFECT_UPDATING, &state->flags);
		state->updated_at = now;
	}

	if(log){
		STDOUT("--upload--\n");
		log_effect(effect);
	}

	if(play_on_upload && !test_bit(FF_EFFECT_PLAYING, &state->flags)){
		lg4ff_play_effect(device, effect->id, 1, log);
	}

	return 0;
}

static void lg4ff_update_state(struct lg4ff_effect_state *state, const Uint64 now)
{
	SDL_HapticEffect *effect = &state->effect;
	Uint64 phase_time;

	if (!test_bit(FF_EFFECT_ALLSET, &state->flags)) {
		state->play_at = state->start_at + get_effect_replay_delay(effect);
		if (!test_bit(FF_EFFECT_UPDATING, &state->flags)) {
			state->updated_at = state->play_at;
		}
		state->direction_gain = sin_deg(effect->direction * 360 / 0x10000);
		if (effect_is_periodic(effect->type)) {
			state->phase_adj = effect->periodic.phase * 360 / effect->periodic.period;
		}
		if (get_effect_replay_length(effect)) {
			state->stop_at = state->play_at + get_effect_replay_length(effect);
		}
	}
	__set_bit(FF_EFFECT_ALLSET, &state->flags);

	if (test_bit(FF_EFFECT_UPDATING, &state->flags)) {
		__clear_bit(FF_EFFECT_PLAYING, &state->flags);
		state->play_at = state->updated_at + get_effect_replay_delay(effect);
		state->direction_gain = sin_deg(effect->direction * 360 / 0x10000);
		if (get_effect_replay_length(effect)) {
			state->stop_at = state->updated_at + get_effect_replay_length(effect);
		}
		if (effect_is_periodic(effect->type)) {
			state->phase_adj = state->phase;
		}
	}
	__clear_bit(FF_EFFECT_UPDATING, &state->flags);

	state->slope = 0;
	if (effect->type == SDL_HAPTIC_RAMP && effect->ramp.length) {
		state->slope = ((effect->ramp.end - effect->ramp.start) << 16) / (effect->ramp.length - state->ramp.attack_length - state->ramp.fade_length);
	}

	if (!test_bit(FF_EFFECT_PLAYING, &state->flags) && time_after_eq(now,
				state->play_at) && (get_effect_replay_length(effect) == 0 ||
					time_before(now, state->stop_at))) {
		__set_bit(FF_EFFECT_PLAYING, &state->flags);
	}

	if (test_bit(FF_EFFECT_PLAYING, &state->flags)) {
		state->time_playing = time_diff(now, state->play_at);
		if (effect_is_periodic(effect->type)) {
			phase_time = time_diff(now, state->updated_at);
			state->phase = (phase_time % effect->periodic.period) * 360 / effect->periodic.period;
			state->phase += state->phase_adj % 360;
		}
	}
}

static Sint32 lg4ff_calculate_constant(struct lg4ff_effect_state *state)
{
   SDL_HapticConstant *constant = &state->effect;
   Sint32 level_sign;
   Sint32 level = constant->level;
   Sint32 d, t;

   if (state->time_playing < constant->attack_length) {
      level_sign = level < 0 ? -1 : 1;
      d = level - level_sign * constant->attack_level;
      level = level_sign * constant->attack_level + d * state->time_playing / constant->attack_length;
   } else if (constant->length) {
      t = state->time_playing - constant->length + constant->fade_length;
      if (t > 0) {
         level_sign = level < 0 ? -1 : 1;
         d = level - level_sign * constant->fade_level;
         level = level - d * t / constant->fade_length;
      }
   }

   return state->direction_gain * level;
}

static Sint32 lg4ff_calculate_ramp(struct lg4ff_effect_state *state)
{
   SDL_HapticRamp *ramp = &state->effect;
	Sint32 level_sign;
	Sint32 level;
	Sint32 d, t;

	if (state->time_playing < ramp->attack_length) {
		level = ramp->start;
		level_sign =  level < 0 ? -1 : 1;
		t = ramp->attack_length - state->time_playing;
		d = level - level_sign * ramp->attack_level;
		level = level_sign * ramp->attack_level + d * t / ramp->attack_length;
	} else if (ramp->length && state->time_playing >= ramp->length - ramp->fade_length) {
		level = ramp->end;
		level_sign = level < 0 ? -1 : 1;
		t = state->time_playing - ramp->length + ramp->fade_length;
		d = level_sign * ramp->fade_level - level;
		level = level - d * t / ramp->fade_length;
	} else {
		t = state->time_playing - ramp->attack_length;
		level = ramp->start + ((t * state->slope) >> 16);
	}

	return state->direction_gain * level;
}

static Sint32 lg4ff_calculate_periodic(struct lg4ff_effect_state *state)
{
   SDL_HapticPeriodic *periodic = &state->effect;
   Sint32 magnitude = periodic->magnitude;
   Sint32 magnitude_sign = magnitude < 0 ? -1 : 1;
   Sint32 level = periodic->offset;
   Sint32 d, t;

   if (state->time_playing < periodic->attack_length) {
      d = magnitude - magnitude_sign * periodic->attack_level;
      magnitude = magnitude_sign * periodic->attack_level + d * state->time_playing / periodic->attack_length;
   } else if (state->effect.replay.length) {
      t = state->time_playing - get_effect_replay_length(&state->effect) + periodic->fade_length;
      if (t > 0) {
         d = magnitude - magnitude_sign * periodic->fade_level;
         magnitude = magnitude - d * t / periodic->fade_length;
      }
   }

   switch (periodic->type) {
      case SDL_HAPTIC_SINE:
         level += sin_deg(state->phase) * magnitude;
         break;
      /*
      case SDL_HAPTIC_SQUARE:
         level += (state->phase < 180 ? 1 : -1) * magnitude;
         break;
      */
      case SDL_HAPTIC_TRIANGLE:
         level += abs(state->phase * magnitude * 2 / 360 - magnitude) * 2 - magnitude;
         break;
      case SDL_HAPTIC_SAWTOOTHUP:
         level += state->phase * magnitude * 2 / 360 - magnitude;
         break;
      case SDL_HAPTIC_SAWTOOTHDOWN:
         level += magnitude - state->phase * magnitude * 2 / 360;
         break;
      default:
         SDL_assert(0);
   }

   return state->direction_gain * level;
}

static void lg4ff_calculate_spring(struct lg4ff_effect_state *state, struct lg4ff_effect_parameters *parameters)
{
	struct ff_condition_effect *condition = &state->effect.u.condition[0];

	parameters->d1 = ((Sint32)condition->center) - condition->deadband / 2;
	parameters->d2 = ((Sint32)condition->center) + condition->deadband / 2;
	parameters->k1 = condition->left_coeff;
	parameters->k2 = condition->right_coeff;
	parameters->clip = (Uint16)condition->right_saturation;
}

static void lg4ff_calculate_resistance(struct lg4ff_effect_state *state, struct lg4ff_effect_parameters *parameters)
{
	struct ff_condition_effect *condition = &state->effect.u.condition[0];

	parameters->k1 = condition->left_coeff;
	parameters->k2 = condition->right_coeff;
	parameters->clip = (Uint16)condition->right_saturation;
}

static void lg4ff_update_slot(struct lg4ff_slot *slot, struct lg4ff_effect_parameters *parameters)
{
	Uint8 original_cmd[7];
	Sint32 d1;
	Sint32 d2;
	Sint32 k1;
	Sint32 k2;
	Sint32 s1;
	Sint32 s2;

	memcpy(original_cmd, slot->current_cmd, sizeof(original_cmd));

	if ((original_cmd[0] & 0xf) == 1) {
		original_cmd[0] = (original_cmd[0] & 0xf0) + 0xc;
	}

	if (slot->effect_type == FF_CONSTANT) {
		if (slot->cmd_op == 0) {
			slot->cmd_op = 1;
		} else {
			slot->cmd_op = 0xc;
		}
	} else {
		if (parameters->clip == 0) {
			slot->cmd_op = 3;
		} else if (slot->cmd_op == 3) {
			slot->cmd_op = 1;
		} else {
			slot->cmd_op = 0xc;
		}
	}

	slot->current_cmd[0] = (0x10 << slot->id) + slot->cmd_op;

	if (slot->cmd_op == 3) {
		slot->current_cmd[1] = 0;
		slot->current_cmd[2] = 0;
		slot->current_cmd[3] = 0;
		slot->current_cmd[4] = 0;
		slot->current_cmd[5] = 0;
		slot->current_cmd[6] = 0;
	} else {
		switch (slot->effect_type) {
			case FF_CONSTANT:
				slot->current_cmd[1] = 0x00;
				slot->current_cmd[2] = 0;
				slot->current_cmd[3] = 0;
				slot->current_cmd[4] = 0;
				slot->current_cmd[5] = 0;
				slot->current_cmd[6] = 0;
				slot->current_cmd[2 + slot->id] = TRANSLATE_FORCE(parameters->level);
				break;
			case FF_SPRING:
				d1 = SCALE_VALUE_U16(((parameters->d1) + 0x8000) & 0xffff, 11);
				d2 = SCALE_VALUE_U16(((parameters->d2) + 0x8000) & 0xffff, 11);
				s1 = parameters->k1 < 0;
				s2 = parameters->k2 < 0;
				k1 = abs(parameters->k1);
				k2 = abs(parameters->k2);
				if (k1 < 2048) {
					d1 = 0;
				} else {
					k1 -= 2048;
				}
				if (k2 < 2048) {
					d2 = 2047;
				} else {
					k2 -= 2048;
				}
				slot->current_cmd[1] = 0x0b;
				slot->current_cmd[2] = d1 >> 3;
				slot->current_cmd[3] = d2 >> 3;
				slot->current_cmd[4] = (SCALE_COEFF(k2, 4) << 4) + SCALE_COEFF(k1, 4);
				slot->current_cmd[5] = ((d2 & 7) << 5) + ((d1 & 7) << 1) + (s2 << 4) + s1;
				slot->current_cmd[6] = SCALE_VALUE_U16(parameters->clip, 8);
				break;
			case FF_DAMPER:
				s1 = parameters->k1 < 0;
				s2 = parameters->k2 < 0;
				slot->current_cmd[1] = 0x0c;
				slot->current_cmd[2] = SCALE_COEFF(parameters->k1, 4);
				slot->current_cmd[3] = s1;
				slot->current_cmd[4] = SCALE_COEFF(parameters->k2, 4);
				slot->current_cmd[5] = s2;
				slot->current_cmd[6] = SCALE_VALUE_U16(parameters->clip, 8);
				break;
			case FF_FRICTION:
				s1 = parameters->k1 < 0;
				s2 = parameters->k2 < 0;
				slot->current_cmd[1] = 0x0e;
				slot->current_cmd[2] = SCALE_COEFF(parameters->k1, 8);
				slot->current_cmd[3] = SCALE_COEFF(parameters->k2, 8);
				slot->current_cmd[4] = SCALE_VALUE_U16(parameters->clip, 8);
				slot->current_cmd[5] = (s2 << 4) + s1;
				slot->current_cmd[6] = 0;
				break;
		}
	}

	if (memcmp(original_cmd, slot->current_cmd, sizeof(original_cmd))) {
		slot->is_updated = 1;
	}
}

int lg4ff_init_slots(struct lg4ff_device *device)
{
   struct lg4ff_effect_parameters parameters;
   Uint8 cmd[7] = {0};
   int i;

   // Set/unset fixed loop mode
   cmd[0] = 0x0d;
   //cmd[1] = fixed_loop ? 1 : 0;
   cmd[1] = 0;
   int ret = SDL_JoystickSendEffect(device->hid_handle, cmd, 7);
   if(ret <= 0){
      return -1;
   }

   SDL_memset(&device->states, 0, sizeof(device->states));
   SDL_memset(&device->slots, 0, sizeof(device->slots));
   SDL_memset(&parameters, 0, sizeof(parameters));

   device->slots[0].effect_type = FF_CONSTANT;
   device->slots[1].effect_type = FF_SPRING;
   device->slots[2].effect_type = FF_DAMPER;
   device->slots[3].effect_type = FF_FRICTION;

   for (i = 0; i < 4; i++) {
      device->slots[i].id = i;
      lg4ff_update_slot(&device->slots[i], &parameters);
      ret = SDL_JoystickSendEffect(device->hid_handle, cmd, 7);
      if(ret <= 0){
         return -1;
      }
      device->slots[i].is_updated = 0;
   }

   return 0;
}

int lg4ff_timer(struct lg4ff_device *device)
{
   struct lg4ff_slot *slot;
   struct lg4ff_effect_state *state;
   struct lg4ff_effect_parameters parameters[4];
   Uint64 now = get_time_ms();
   Uint16 gain;
   Sint32 current_period;
   Sint32 count;
   Sint32 effect_id;
   int i;
   Sint32 ffb_level;

   // XXX how to detect stacked up effects here?

   SDL_memset(parameters, 0, sizeof(parameters));

   gain = (Uint32)device->gain * device->app_gain / 0xffff;

   count = device->effects_used;

   for (effect_id = 0; effect_id < LG4FF_MAX_EFFECTS; effect_id++) {

      if (!count) {
         break;
      }

      state = &device->states[effect_id];

      if (!test_bit(FF_EFFECT_STARTED, &state->flags)) {
         continue;
      }

      count--;

      if (test_bit(FF_EFFECT_ALLSET, &state->flags)) {
         if (state->effect.replay.length && time_after_eq(now, state->stop_at)) {
            STOP_EFFECT(state);
            if (!--state->count) {
               device->effects_used--;
               continue;
            }
            __set_bit(FF_EFFECT_STARTED, &state->flags);
            state->start_at = state->stop_at;
         }
      }

      lg4ff_update_state(state, now);

      if (!test_bit(FF_EFFECT_PLAYING, &state->flags)) {
         continue;
      }

      if (effect_is_periodic(&state->effect)) {
         parameters[0].level += lg4ff_calculate_periodic(state);
      } else {
         switch (state->effect.type) {
            case FF_CONSTANT:
               parameters[0].level += lg4ff_calculate_constant(state);
               break;
            case FF_RAMP:
               parameters[0].level += lg4ff_calculate_ramp(state);
               break;
            case FF_SPRING:
               lg4ff_calculate_spring(state, &parameters[1]);
               break;
            case FF_DAMPER:
               lg4ff_calculate_resistance(state, &parameters[2]);
               break;
            case FF_FRICTION:
               lg4ff_calculate_resistance(state, &parameters[3]);
               break;
         }
      }
   }

   parameters[0].level = (Sint64)parameters[0].level * gain / 0xffff;
   parameters[1].clip = parameters[1].clip * device->spring_level / 100;
   parameters[2].clip = parameters[2].clip * device->damper_level / 100;
   parameters[3].clip = parameters[3].clip * device->friction_level / 100;

   ffb_level = abs(parameters[0].level);
   for (i = 1; i < 4; i++) {
      parameters[i].k1 = (Sint64)parameters[i].k1 * gain / 0xffff;
      parameters[i].k2 = (Sint64)parameters[i].k2 * gain / 0xffff;
      parameters[i].clip = parameters[i].clip * gain / 0xffff;
      ffb_level += parameters[i].clip * 0x7fff / 0xffff;
   }
   if (ffb_level > device->peak_ffb_level) {
      device->peak_ffb_level = ffb_level;
   }

   for (i = 0; i < 4; i++) {
      slot = &device->slots[i];
      lg4ff_update_slot(slot, &parameters[i]);
      if (slot->is_updated) {
         int ret = hid_write(device->hid_handle, slot->current_cmd, 7);
         if(ret == -1){
            char err_buf[128];
            wcstombs(err_buf, hid_error(device->hid_handle), sizeof(err_buf));
            STDERR("failed sending effect command, %s\n", err_buf);
            exit(1);
         }
         slot->is_updated = 0;
      }
   }

   return 0;
}

static SDL_bool SDL_HIDAPI_HapticDriverLg4ff_JoystickSupported(SDL_Joystick *joystick)
{
   Uint16 vendor_id = SDL_JoystickGetVendor(joystick);
   Uint16 product_id = SDL_JoystickGetProduct(joystick);
   if (vendor_id != USB_VENDOR_ID_LOGITECH) {
      return SDL_FALSE;
   }
   for (int i = 0;i < sizeof(supported_device_ids) / sizeof(Uint32);i++) {
      if (supported_device_ids[i] == product_id) {
         return SDL_TRUE;
      }
   }
   return SDL_FALSE;
}

static int SDLCALL SDL_HIDAPI_HapticDriverLg4ff_ThreadFunction(void *ctx_in)
{
   lg4ff_device *ctx = (lg4ff_device *)ctx_in;
   while (SDL_TRUE) {
      SDL_LockMutex(ctx->mutex);
      lg4ff_timer();
      SDL_UnlockMutex(ctx->mutex);
      SDL_Delay(2);
   }
}

static void *SDL_HIDAPI_HapticDriverLg4ff_Open(SDL_Joystick *joystick)
{
   if (!SDL_HIDAPI_JoystickSupported(joystick)) {
      SDL_SetError("Device not supported by the lg4ff hidapi haptic driver");
      return NULL;
   }

   lg4ff_device *ctx = SDL_malloc(sizeof(SDL_HIDAPI_HapticDriverLg4ff_Context));
   if (ctx == NULL) {
        SDL_OutOfMemory();
        return NULL;
   }
   SDL_memset(ctx, 0, sizeof(lg4ff_device));

   ctx->hid_handle = joystick;
   if (lg4ff_init_slots(ctx) != 0) {
      SDL_Error("lg4ff hidapi driver failed initializing effect slots");
      SDL_free(ctx);
      return NULL;
   }

   ctx->mutex = SDL_CreateMutex();
   if (ctx->mutex == NULL) {
      SDL_free(ctx);
      return NULL;
   }

   ctx->product_id = SDL_JoystickGetProduct(joystick);

   sprintf(ctx->thread_name_buf, "SDL_hidapihaptic_lg4ff 0x%16x %04x:%04x", joystick, USB_VENDOR_ID_LOGITECH, ctx->product_id);
   ctx->stop_thread = SDL_FALSE;
   ctx->thread = SDL_CreateThread(SDL_HIDAPI_HapticDriverLg4ff_ThreadFunction, ctx->thread_name_buf);

   return ctx;
}

SDL_HIDAPI_HapticDriver SDL_HIDAPI_HapticDriverLg4ff = {
   SDL_HIDAPI_HapticDriverLg4ff_JoystickSupported,
   SDL_HIDAPI_HapticDriverLg4ff_Open,
};

#endif //SDL_HAPTIC_HIDAPI_LG4FF
#endif //SDL_JOYSTICK_HIDAPI