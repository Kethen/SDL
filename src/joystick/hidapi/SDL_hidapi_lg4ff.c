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

/*
  This driver is heavily based on https://github.com/berarma/new-lg4ff
*/

#include "../../SDL_internal.h"

#ifdef SDL_JOYSTICK_HIDAPI

#include "../SDL_sysjoystick.h"
#include "SDL_events.h"
#include "SDL_hidapijoystick_c.h"

#ifdef SDL_JOYSTICK_HIDAPI_LG4FF

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

// keep the same order as the supported_ids array
static const char* supported_device_names[] = {
    "Logitech G29",
    "Logitech G27",
    "Logitech G25",
    "Logitech Driving Force GT",
    "Logitech Driving Force Pro",
    "Driving Force EX"
};

static get_device_name(Uint32 device_id){
    for (int i = 0;i < (sizeof supported_device_ids) / sizeof(Uint32);i++) {
        if (supported_device_ids[i] == device_id) {
            return supported_device_names[i];
        }
    }
    SDL_assert(0);
    return "";
}

typedef struct
{
    Uint8 last_report_buf[8];
} SDL_DriverLg4ff_Context;

static void HIDAPI_DriverLg4ff_RegisterHints(SDL_HintCallback callback, void *userdata)
{
    SDL_AddHintCallback(SDL_HINT_JOYSTICK_HIDAPI_LG4FF, callback, userdata);
}

static void HIDAPI_DriverLg4ff_UnregisterHints(SDL_HintCallback callback, void *userdata)
{
    SDL_DelHintCallback(SDL_HINT_JOYSTICK_HIDAPI_LG4FF, callback, userdata);
}

static SDL_bool HIDAPI_DriverLg4ff_IsEnabled(void)
{
    return SDL_GetHintBoolean(SDL_HINT_JOYSTICK_HIDAPI_LG4FF,
                              SDL_GetHintBoolean(SDL_HINT_JOYSTICK_HIDAPI, SDL_HIDAPI_DEFAULT));
}

static SDL_bool HIDAPI_DriverLg4ff_IsSupportedDevice(
    SDL_HIDAPI_Device *device,
    const char *name,
    SDL_GameControllerType type,
    Uint16 vendor_id,
    Uint16 product_id,
    Uint16 version,
    int interface_number,
    int interface_class,
    int interface_subclass,
    int interface_protocol)
{
    /*
      TODO
      
      Is it possible to identify native mode from hid? On the Linux kernel
      driver that is done by checking with the usb stack, more specifically 
      bcdDevice on the usb descriptor

      If a way is found to probe for native mode on the HID layer, this function
      should trigger mode switch, then return false, so the next probe cycle
      would take the device on a supported mode
    */
    if (product_id != USB_VENDOR_ID_LOGITECH) {
        return SDL_FALSE;
    }
    for (i = 0;i < sizeof(supported_device_ids) / sizeof(Uint32);i++) {
        if (supported_device_ids[i] == vendor_id) {
            return SDL_TRUE;
        }
    }
    return SDL_FALSE;
}

static SDL_bool HIDAPI_DriverLg4ff_InitDevice(SDL_HIDAPI_Device *device)
{
    SDL_DriverSteamDeck_Context *ctx;

    ctx = (SDL_DriverSteamDeck_Context *)SDL_calloc(1, sizeof(SDL_DriverSteamDeck_Context));
    if (ctx == NULL) {
        SDL_OutOfMemory();
        return SDL_FALSE;
    }
    SDL_memset(ctx, 0, sizeof(SDL_DriverSteamDeck_Context));

    device->context = ctx;

    HIDAPI_SetDeviceName(device, get_device_name(device->product_id));

    if (SDL_hid_set_nonblocking(device, 1) != 0) {
        return SDL_FALSE;
    }

    return HIDAPI_JoystickConnected(device, NULL);
}

static int HIDAPI_DriverLg4ff_GetDevicePlayerIndex(SDL_HIDAPI_Device *device, SDL_JoystickID instance_id)
{
    return -1;
}

static void HIDAPI_DriverLg4ff_SetDevicePlayerIndex(SDL_HIDAPI_Device *device, SDL_JoystickID instance_id, int player_index)
{
}

static void HIDAPI_DriverLg4ff_HandleState(SDL_HIDAPI_Device *device,
                                               SDL_Joystick *joystick,
                                               Uint8 *report_buf)
{
    SDL_DriverLg4ff_Context *ctx = (SDL_DriverLg4ff_Context *)device->context;

    Uint8 hat = 0;
    Uint8 last_hat = 0;
    
    switch (device->product_id) {
        case USB_DEVICE_ID_LOGITECH_G29_WHEEL:
        case USB_DEVICE_ID_LOGITECH_G27_WHEEL:
        case USB_DEVICE_ID_LOGITECH_G25_WHEEL:
        case USB_DEVICE_ID_LOGITECH_DFGT_WHEEL:
            hat = report_buf[0] & 0x0f;
            old_hat = ctx->last_report_buf[0] 0x0f;
            break;
        case USB_DEVICE_ID_LOGITECH_DFP_WHEEL:
            hat = report_buf[3] >> 4;
            last_hat = ctx->last_report_buf[3] >> 4;
            break;
        case USB_DEVICE_ID_LOGITECH_WHEEL:
            hat = report_buf[2] & 0x0F;
            last_hat = ctx->last_report_buf[2] & 0x0F;
        default:
            SDL_assert(0);
    }

    if (hat != last_hat) {
        Uint8 sdl_hat = 0;
        switch (hat) {
            case 0:
                sdl_hat = SDL_HAT_UP;
                break;
            case 1:
                sdl_hat = SDL_HAT_RIGHTUP;
                break;
            case 2:
                sdl_hat = SDL_HAT_RIGHT;
                break;
            case 3:
                sdl_hat = SDL_HAT_RIGHTDOWN;
                break;
            case 4:
                sdl_hat = SDL_HAT_DOWN;
                break;
            case 5:
                sdl_hat = SDL_HAT_LEFTDOWN;
                break;
            case 6:
                sdl_hat = SDL_HAT_LEFT;
                break;
            case 7:
                sdl_hat = SDL_HAT_LEFTUP;
                break;
            case 8:
                sdl_hat = SDL_HAT_CENTERED;
                break;
            // XXX do not assert out, in case hardware can report weird hat values
        }
        SDL_PrivateJoystickHat(joystick, 0, sdl_hat);
    }

}

static SDL_bool HIDAPI_DriverLg4ff_UpdateDevice(SDL_HIDAPI_Device *device)
{
    SDL_Joystick *joystick = NULL;
    size_t r;
    Uint8 report_buf[32];

    if (device->num_joysticks > 0) {
        joystick = SDL_JoystickFromInstanceID(device->joysticks[0]);
        if (joystick == NULL) {
            return SDL_FALSE;
        }
    } else {
        return SDL_FALSE;
    }

    SDL_memset(data, 0, sizeof(report_buf));

    size_t report_size = 0;
    switch (device->product_id) {
        case USB_DEVICE_ID_LOGITECH_G29_WHEEL:
            report_size = 12;
            break;
        case USB_DEVICE_ID_LOGITECH_G27_WHEEL:
        case USB_DEVICE_ID_LOGITECH_G25_WHEEL:
        case USB_DEVICE_ID_LOGITECH_DFGT_WHEEL:
        case USB_DEVICE_ID_LOGITECH_DFP_WHEEL:
        case USB_DEVICE_ID_LOGITECH_WHEEL:
            report_size = 8;
            break;
        default:
            SDL_assert(0);
    }

    do {
        r = SDL_hid_read(device->dev, report_buf, report_size);

        if (r < 0) {
            /* Failed to read from controller */
            HIDAPI_JoystickDisconnected(device, device->joysticks[0]);
            return SDL_FALSE;
        } else if (r == report_size) {
            HIDAPI_DriverSteamDeck_HandleState(device, joystick, report_buf);
        }
    } while (r > 0);

    return SDL_TRUE;
}
/*
  XXX

  Once again the Linux driver checks between ffex and dfex on the usb
  stack, not sure how one can check for that on hid.
*/

SDL_HIDAPI_DeviceDriver SDL_HIDAPI_DriverLg4ff = {
    SDL_HINT_JOYSTICK_HIDAPI_LG4FF,
    SDL_TRUE,
    HIDAPI_DriverLg4ff_RegisterHints,
    HIDAPI_DriverLg4ff_UnregisterHints,
    HIDAPI_DriverLg4ff_IsEnabled,
    HIDAPI_DriverLg4ff_IsSupportedDevice,
    HIDAPI_DriverLg4ff_InitDevice,
    HIDAPI_DriverLg4ff_GetDevicePlayerIndex,
    HIDAPI_DriverLg4ff_SetDevicePlayerIndex,
    HIDAPI_DriverSteamDeck_UpdateDevice,
    HIDAPI_DriverSteamDeck_OpenJoystick,
    HIDAPI_DriverSteamDeck_RumbleJoystick,
    HIDAPI_DriverSteamDeck_RumbleJoystickTriggers,
    HIDAPI_DriverSteamDeck_GetJoystickCapabilities,
    HIDAPI_DriverSteamDeck_SetJoystickLED,
    HIDAPI_DriverSteamDeck_SendJoystickEffect,
    HIDAPI_DriverSteamDeck_SetSensorsEnabled,
    HIDAPI_DriverSteamDeck_CloseJoystick,
    HIDAPI_DriverSteamDeck_FreeDevice,
};


#endif /* SDL_JOYSTICK_HIDAPI_LG4FF */

#endif /* SDL_JOYSTICK_HIDAPI */