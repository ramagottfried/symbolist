/***************************************************/
/*!
 *  @file       symbolist.cpp
 *  @brief      L'brary for symbolic score display and editing
 *  @author     Jean Bresson & Rama Gottfried
 *  @date       10/05/2017
 */
/***************************************************/
#include "symbolist.hpp"
#include "MainComponent.h"

const char* symbolistInfo()
{
    return "symbolist v.0.1";
}

void *symbolistNewWindow()
{
    return SymbolistMainComponent::symbolistAPI_createWindow();
}

void symbolistCloseWindow(void* maincomponent)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->symbolistAPI_closeWindow();
}

void symbolistWindowToFront(void* maincomponent)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->symbolistAPI_windowToFront();
}

void symbolistWindowSetName(void* maincomponent, char *name)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->symbolistAPI_windowSetName(String(name));
}

void symbolistRegisterCloseCallback(void* maincomponent, symbolistCloseCallback callback)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->symbolistAPI_registerCloseCallback(callback);
}

void symbolistRegisterUpdateCallback(void* maincomponent, symbolistUpdateCallback callback)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->symbolistAPI_registerUpdateCallback(callback);
}

void symbolistRegisterTransportCallback(void* maincomponent, symbolistTransportCallback callback)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->symbolistAPI_registerTransportCallback(callback);
}

int symbolistGetNumSymbols(void* maincomponent)
{
    return static_cast<SymbolistMainComponent*>(maincomponent)->symbolistAPI_getNumSymbols();
}

odot_bundle* symbolistGetSymbol(void* maincomponent, int n)
{
    return static_cast<SymbolistMainComponent*>(maincomponent)->symbolistAPI_getSymbol(n);
}

void symbolistSetSymbols(void* maincomponent, int n, odot_bundle **bundle_array)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->symbolistAPI_setSymbols(n, bundle_array);
}

void symbolistSetTime(void* maincomponent, int time_ms)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->symbolistAPI_setTime(time_ms);
}



