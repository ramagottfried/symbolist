
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
    main_component = nullptr;
    cout << "freeing main window " << this << " allocated main_component is now " << main_component << endl;
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

    if( sh->isStandalone() )
    {
        cout << "closeButtonPressed freeing" << endl;
        sh->symbolistAPI_closeWindow();
        sh = nullptr;        

        // could have several open windows maybe later, and then we wouldn't request Quit here
//        JUCEApplication::getInstance()->systemRequestedQuit();

    }
    else
    {
        sh->symbolistAPI_closeWindow();
        sh->executeCloseCallback();
    }
    
}

