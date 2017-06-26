/***************************************************/
/*!
 *  @file       symbolist.cpp
 *  @brief      L'brary for symbolic score display and editing
 *  @author     Jean Bresson & Rama Gottfried
 *  @date       10/05/2017
 */
/***************************************************/
#include "symbolist.hpp"
#include "SymbolistHandler.h"

const char* symbolistInfo()
{
    return "symbolist v.0.1";
}

void* symbolistNew()
{
    return SymbolistHandler::symbolistAPI_newSymbolist();
}

void symbolistFree(void* symbolist_handler)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_freeSymbolist();
}

void symbolistOpenWindow(void* symbolist_handler)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_openWindow();
}

void symbolistCloseWindow(void* symbolist_handler)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_closeWindow();
}

void symbolistWindowToFront(void* symbolist_handler)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_windowToFront();
}

void symbolistWindowSetName(void* symbolist_handler, char *name)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_windowSetName(String(name));
}

void symbolistRegisterCloseCallback(void* symbolist_handler, symbolistCloseCallback callback)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_registerCloseCallback(callback);
}

void symbolistRegisterUpdateCallback(void* symbolist_handler, symbolistUpdateCallback callback)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_registerUpdateCallback(callback);
}

void symbolistRegisterTransportCallback(void* symbolist_handler, symbolistTransportCallback callback)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_registerTransportCallback(callback);
}

int symbolistGetNumSymbols(void* symbolist_handler)
{
    return static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_getNumSymbols();
}

odot_bundle* symbolistGetSymbol(void* symbolist_handler, int n)
{
    return static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_getSymbol(n);
}

void symbolistSetSymbols(void* symbolist_handler, int n, odot_bundle **bundle_array)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_setSymbols(n, bundle_array);
}

void symbolistSetOneSymbol(void* symbolist_handler, odot_bundle *bundle)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_setOneSymbol( bundle );
}

void symbolistClearScore(void* symbolist_handler)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_clearScore();
}

void symbolistSetTime(void* symbolist_handler, float time_ms)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_setTime(time_ms);
}

odot_bundle* symbolistGetSymbolsAtTime(void* symbolist_handler, float t)
{
    return static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_getSymbolsAtTime( t );
}

odot_bundle* symbolistGetScoreBundle(void* symbolist_handler )
{
    return static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_getScoreBundle();
}


