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
    std::cout << "freeing and nulling void pointer " << symbolist_handler << std::endl;
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_freeSymbolist();
    symbolist_handler = nullptr;
    std::cout << "void* now " << symbolist_handler << std::endl;
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

t_osc_bndl_s * symbolistGetSymbol(void* symbolist_handler, int n)
{
    return static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_getSymbolBundle_s(n).release();
}
void symbolistSetOneSymbol(void* symbolist_handler, t_osc_bndl_s *bundle)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_setOneSymbol( OdotBundle_s(bundle) );
}

void symbolistSetSymbols(void* symbolist_handler, t_osc_bndl_s *bundle_array)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_setSymbols( OdotBundle_s(bundle_array) );
}

int symbolistGetNumPaletteSymbols(void* symbolist_handler)
{
    return static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_getNumPaletteSymbols();
}

t_osc_bndl_s* symbolistGetPaletteSymbol(void* symbolist_handler, int n)
{
    return static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_getPaletteSymbol(n)->serialize().release();
}

void symbolistSetOnePaletteSymbol(void* symbolist_handler, t_osc_bndl_s *bundle)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_setOnePaletteSymbol( bundle );
}

void symbolistSetPaletteSymbols(void* symbolist_handler, t_osc_bndl_s *bundle_array)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_setPaletteSymbols( OdotBundle_s(bundle_array) );
}

void symbolistClearScore(void* symbolist_handler)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_clearScore();
}

void symbolistSetTime(void* symbolist_handler, float time_ms)
{
    static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_setTime(time_ms);
}

t_osc_bndl_s* symbolistGetDurationBundle(void* symbolist_handler)
{
    return static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_getdurationBundle().release();
}


t_osc_bndl_s* symbolistGetSymbolsAtTime(void* symbolist_handler, float t)
{
    return static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_getSymbolsAtTime( t ).release();
}

t_osc_bndl_s* symbolistGetScoreBundle(void* symbolist_handler )
{
    return static_cast<SymbolistHandler*>(symbolist_handler)->symbolistAPI_getScoreBundle().release();
}


