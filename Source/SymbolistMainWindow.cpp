#include "types.h"
#include "SymbolistMainWindow.h"

static ScopedPointer<ApplicationCommandManager> applicationCommandManager;

SymbolistMainWindow::SymbolistMainWindow () :
    DocumentWindow ("symbolist", Colours::white, DocumentWindow::allButtons)
{
    cout << "SymbolistMainWindow's default constructor." << endl;
    
    // default stuff copied from Juce
    setUsingNativeTitleBar (true);
    main_component = new SymbolistMainComponent();
    
    setContentOwned(main_component, true);
    
    centreWithSize(getWidth(), getHeight());
    setVisible (true);
    setResizable(true, true);
    
    addKeyListener(getApplicationCommandManager().getKeyMappings());
    
    triggerAsyncUpdate();

}

SymbolistMainWindow::~SymbolistMainWindow ()
{
    cout << "SymbolistMainWindow's destructor." << endl;
    
    main_component = nullptr;
    applicationCommandManager = nullptr;

    cout << "Freeing main window " << this << " allocated main_component is now " << main_component << endl;
    
}

ApplicationCommandManager& SymbolistMainWindow::getApplicationCommandManager()
{
    if (applicationCommandManager == nullptr)
        applicationCommandManager = new ApplicationCommandManager();
    
    return *applicationCommandManager;
}

void SymbolistMainWindow::handleAsyncUpdate()
{
    // cout << "SymbolistMainWindow::handleAsyncUpdate " << endl;
    
    // This registers all of our commands with the command manager but has to be done after the window has
    // been created so we can find the number of rendering engines available
    auto& commandManager = SymbolistMainWindow::getApplicationCommandManager();
    
    commandManager.registerAllCommandsForTarget(main_component);
    commandManager.registerAllCommandsForTarget(JUCEApplication::getInstance());
    
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

