
#include "MainWindow.h"


/************************************************
 * SHARED BY THE LIBRARY AND THE STANDALONE APP
 ************************************************/

SymbolistMainWindow::SymbolistMainWindow ( Score *s ): DocumentWindow ( "symbolist",
                                                                Desktop::getInstance().getDefaultLookAndFeel().findColour(ResizableWindow::backgroundColourId),
                                                                DocumentWindow::allButtons)
{
    
    score = s;
    
    setUsingNativeTitleBar (true);
    setContentOwned ( new MainComponent( score ), true );
    
    centreWithSize (getWidth(), getHeight());
    setVisible (true);
    setResizable(true, true);

    
}

SymbolistMainWindow::SymbolistMainWindow () : SymbolistMainWindow( new Score () ) {}


SymbolistMainWindow::~SymbolistMainWindow() {}



/***********************************
 * SPECIFIC FOR THE LIBRARY
 ***********************************/


void SymbolistEditorWindow::registerCloseCallback(symbolistCloseCallback c) { myCloseCallback = c; }
void SymbolistEditorWindow::registerUpdateCallback(symbolistUpdateCallback c) { myUpdateCallback = c; }


void SymbolistEditorWindow::closeButtonPressed() {

    if (myCloseCallback) { myCloseCallback( this ); }
    delete this;

}


