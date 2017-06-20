
#include "types.h"
#include "SymbolistMainWindow.h"


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

SymbolistMainWindow::~SymbolistMainWindow () {}


SymbolistMainComponent* SymbolistMainWindow::getMainComponent()
{
    return &main_component;
}


void SymbolistMainWindow::closeButtonPressed()
{
    //JUCEApplication::getInstance()->systemRequestedQuit();
    ////delete this;
    main_component.getSymbolistHandler()->symbolistAPI_closeWindow();
    main_component.getSymbolistHandler()->executeCloseCallback();
    
}

