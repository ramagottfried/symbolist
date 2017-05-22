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

const char* symbolistInfo() { return "symbolist v.0.1"; }

int symbolistInit() { return 1; }
int symbolistExit() { return 0; }

void *symbolistNewWindow()
{
    return SymbolistMainComponent::createWindow();
}

/*
void *symbolistNewWindowWithSymbols(int n, odot_bundle **bundle_array)
{
    SymbolistMainComponent *s = SymbolistMainComponent::createWindow();
    w->getScore()->importScoreFromOSC(n, bundle_array);
    w->updateViewer();
    return new SymbolistEditorWindow( new Score( n, bundle_array) );
}
 */

void symbolistCloseWindow(void* maincomponent)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->closeWindow();
}

void symbolistWindowToFront(void* maincomponent)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->windowToFront();
}

void symbolistWindowSetName(void* maincomponent, char *name)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->windowSetName(String(name));
}

void symbolistRegisterCloseCallback(void* maincomponent, symbolistCloseCallback callback)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->registerCloseCallback(callback);
}

void symbolistRegisterUpdateCallback(void* maincomponent, symbolistUpdateCallback callback)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->registerUpdateCallback(callback);
}


int symbolistGetNumSymbols(void* maincomponent)
{
    return static_cast<SymbolistMainComponent*>(maincomponent)->getScore()->getSize();
}

odot_bundle* symbolistGetSymbol(void* maincomponent, int n)
{
    return static_cast<SymbolistMainComponent*>(maincomponent)->getScore()->getSymbol(n)->exportToOSC();
}

void symbolistSetSymbols(void* maincomponent, int n, odot_bundle **bundle_array)
{
    static_cast<SymbolistMainComponent*>(maincomponent)->clearScoreView();
    static_cast<SymbolistMainComponent*>(maincomponent)->getScore()->importScoreFromOSC(n, bundle_array);
    static_cast<SymbolistMainComponent*>(maincomponent)->setContentFromScore();
}



