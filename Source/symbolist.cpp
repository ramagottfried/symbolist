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
#include "ScoreData.h"

int symbolistInit() {
    return 1; // went ok
};

int symbolistExit() {
    return 1; // went ok
}

void setupTestScore(Score& s)
{
    System *sys = new System();;
    s.addSystem( sys );
}

void *symbolistTest() {
    
    Score score();
    
    MainWindow *w = new MainWindow ( "test", score );
    return w; // went ok

}

const char* symbolistInfo() {
    return "symbolist v.0.1";
}






