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

void setupTestScore(Score& s)
{
    System *sys = new System();
    s.addSystem( sys );
}

void *symbolistNewWindow()
{
    return new MainWindow ();
}

void *symbolistNewWindowWithSymbols(int n, void *bundle_array) {
    
    Score *s = new Score( n, bundle_array );

    return new MainWindow( s );
}








