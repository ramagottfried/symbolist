
#include "types.h"
#include "MainWindow.h"

/************************************************
 * SHARED BY THE LIBRARY AND THE STANDALONE APP
 ************************************************/

SymbolistMainWindow::SymbolistMainWindow (SymbolistHandler *sh) : DocumentWindow ( "symbolist",
                                                                Desktop::getInstance().getDefaultLookAndFeel().findColour(ResizableWindow::backgroundColourId),
                                                                DocumentWindow::allButtons),
                                            main_component(sh)
{
    // default stuff copied from Juce
    setUsingNativeTitleBar (true);
    setContentOwned (&main_component , true );
    centreWithSize (getWidth(), getHeight());
    setVisible (true);
    setResizable(true, true);
}

void SymbolistMainWindow::closeButtonPressed()
{
    main_component.close();
}


/***********************************
 * SPECIFIC FOR THE LIBRARY
 ***********************************/



void SymbolistEditorWindow::closeButtonPressed()
{
    SymbolistMainWindow::closeButtonPressed();
    delete this;
}


/***********************************
 * SPECIFIC FOR THE STANDALONE APP
 ***********************************/

void AppMainWindow::closeButtonPressed()
{
    SymbolistMainWindow::closeButtonPressed();
    JUCEApplication::getInstance()->systemRequestedQuit();
}
