/***************************************************/
/*!
 *  @file       symbolist.cpp
 *  @brief      Laibrary for symbolic score display and editing
 *  @author     Jean Bresson & Rama Gottfried
 *  @date       10/05/2017
 */
/***************************************************/
#include "symbolist.hpp"
#include "MainWindow.h"

const char* symbolistInfo() {
    return "symbolist v.0.1";
}

int symbolistInit() {
    return 1; // went ok
};

int symbolistExit() {
    return 1; // went ok
}

void *symbolistNewWindow() {
    return new MainWindow();
}

void *symbolistNewWindowWithSymbols(int n, void *bundle_array) {
    return new MainWindow(n, bundle_array);
}








