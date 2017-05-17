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

const char* symbolistInfo() {
    return "symbolist v.0.1";
}

int symbolistInit() {
    return 1; // went ok
};

int symbolistExit() {
    return 1; // went ok
}

void *symbolistNewWindow()
{
    return new SymbolistEditorWindow ();
}

void *symbolistNewWindowWithSymbols(int n, odot_bundle **bundle_array) {
    Score *s = new Score( n, bundle_array) ;
    return new SymbolistEditorWindow( s );
}

void symbolistRegisterCloseCallback(void* window, symbolistCloseCallback callback){
    static_cast<SymbolistEditorWindow*>(window)->registerCloseCallback(callback);
}

void symbolistRegisterUpdateCallback(void* window, symbolistUpdateCallback callback){
    static_cast<SymbolistEditorWindow*>(window)->registerUpdateCallback(callback);
}

void symbolistWindowToFront(void* window){
    static_cast<SymbolistEditorWindow*>(window)->toFront(true);
}

void symbolistWindowSetName(void* window, char *name){
    static_cast<SymbolistEditorWindow*>(window)->setName(String(name));
}

void symbolistWindowUpdateSymbols(void* window, int n, odot_bundle **bundle_array){
    // static_cast<SymbolistEditorWindow*>(window)->updateSymbols(n, bundle_array);
}


