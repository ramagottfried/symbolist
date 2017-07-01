
#include "types.h"
#include "SymbolistMainWindow.h"


SymbolistMainWindow::SymbolistMainWindow (SymbolistHandler *sh) : DocumentWindow ( "symbolist",
                                                                Desktop::getInstance().getDefaultLookAndFeel().findColour(ResizableWindow::backgroundColourId),
                                                                DocumentWindow::allButtons)
{
    // default stuff copied from Juce
    setUsingNativeTitleBar (true);
    main_component = new SymbolistMainComponent(sh);
    setContentOwned (main_component , true );
    centreWithSize (getWidth(), getHeight());
    setVisible (true);
    setResizable(true, true);
}

SymbolistMainWindow::~SymbolistMainWindow ()
{
    cout << "freeing main window" << endl;
}


SymbolistMainComponent* SymbolistMainWindow::getMainComponent()
{
    return main_component;
}


void SymbolistMainWindow::closeButtonPressed()
{
    ////delete this;
    
    SymbolistHandler* sh = main_component->getSymbolistHandler();
    sh->symbolistAPI_closeWindow();

    if(sh->isStandalone() )
    {
        cout << "closeButtonPressed freeing" << endl;
        sh->symbolistAPI_closeWindow();
        sh->symbolistAPI_freeSymbolist();
        sh = nullptr;
        JUCEApplication::getInstance()->systemRequestedQuit();

    }
    else
    {
        sh->symbolistAPI_closeWindow();
        sh->executeCloseCallback();
    }
    
}

