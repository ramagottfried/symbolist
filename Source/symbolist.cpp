/***************************************************/
/*!
 *  @file       symbolist.cpp
 *  @brief      Laibrary for symbolic score display and editing
 *  @author     Jean Bresson & Rama Gottfried
 *  @date       10/05/2017
 */
/***************************************************/
#include "symbolist.hpp"
#include "MainWindow.hpp"


int symbolistInit() {
    return 1; // went ok
};

int symbolistExit() {
    return 1; // went ok
}

int symbolistTest() {
    
    MainWindow *w = new MainWindow("test");
    return 1; // went ok

}

const char* symbolistInfo() {
    return "symbolist v.0.1";
}






