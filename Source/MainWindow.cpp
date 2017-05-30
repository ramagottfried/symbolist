
#include "types.h"
#include "MainWindow.h"

/************************************************
 * SHARED BY THE LIBRARY AND THE STANDALONE APP
 ************************************************/

SymbolistMainWindow::SymbolistMainWindow () : DocumentWindow ( "symbolist",
                                                                Desktop::getInstance().getDefaultLookAndFeel().findColour(ResizableWindow::backgroundColourId),
                                                                DocumentWindow::allButtons)
{
    // default stuff copied from Juce
    setUsingNativeTitleBar (true);
    setContentOwned (&main_component , true );
    centreWithSize (getWidth(), getHeight());
    setVisible (true);
    setResizable(true, true);
}


/***********************************
 * SPECIFIC FOR THE LIBRARY
 ***********************************/

void SymbolistEditorWindow::closeButtonPressed()
{
    main_component.executeCloseCallback();
    delete this;
}

