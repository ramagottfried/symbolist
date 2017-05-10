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


int symbolistInit() {
    return 1; // went ok
};

int symbolistExit() {
    return 1; // went ok
}

void *symbolistTest() {
    
    MainWindow *w = new MainWindow("test");
    return w; // went ok

}

const char* symbolistInfo() {
    return "symbolist v.0.1";
}






