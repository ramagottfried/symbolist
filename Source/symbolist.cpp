/***************************************************/
/*!
 *  @file       symbolist.cpp
 *  @brief      L'brary for symbolic score display and editing
 *  @author     Jean Bresson & Rama Gottfried
 *  @date       10/05/2017
 */
/***************************************************/
#include "symbolist.hpp"
#include "MainWindow.h"
#include "ScoreData.h"

const char* symbolistInfo()
{
    return "symbolist v.0.1";
}

int symbolistInit() { return 1; }
int symbolistExit() { return 0; }

void *symbolistNewWindow()
{
    return new SymbolistEditorWindow ();
}

void *symbolistNewWindowWithSymbols(int n, odot_bundle **bundle_array)
{
    return new SymbolistEditorWindow( new Score( n, bundle_array) );
}

void symbolistRegisterCloseCallback(void* window, symbolistCloseCallback callback)
{
    static_cast<SymbolistEditorWindow*>(window)->registerCloseCallback(callback);
}

void symbolistRegisterUpdateCallback(void* window, symbolistUpdateCallback callback)
{
    static_cast<SymbolistEditorWindow*>(window)->registerUpdateCallback(callback);
}

void symbolistWindowToFront(void* window)
{
    static_cast<SymbolistEditorWindow*>(window)->toFront(true);
}

void symbolistWindowSetName(void* window, char *name)
{
    static_cast<SymbolistEditorWindow*>(window)->setName(String(name));
}

int symbolistGetNumSymbols(void* window)
{
    return static_cast<SymbolistEditorWindow*>(window)->getScore()->getSize();
}

odot_bundle* symbolistGetSymbol(void* window, int n)
{
    return static_cast<SymbolistEditorWindow*>(window)->getScore()->getSymbol(n)->exportToOSC();
}

void symbolistSetSymbols(void* window, int n, odot_bundle **bundle_array)
{
    SymbolistEditorWindow* w = static_cast<SymbolistEditorWindow*>(window);
    cout << "clear..." << endl;
    w->clearViewer();
    cout << "import..." << endl;
    w->getScore()->importScoreFromOSC(n, bundle_array);
    cout << "update..." << endl;
    w->updateViewer();
}



