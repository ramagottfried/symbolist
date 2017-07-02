/*
  ==============================================================================

    This file was auto-generated!

    It contains the basic startup code for a Juce application.

  ==============================================================================
*/

#include "../JuceLibraryCode/JuceHeader.h"
//#include "SymbolistHandler.h"
#include "symbolist.hpp"


//==============================================================================
class SymbolistApplication  : public JUCEApplication
{
public:
    //==============================================================================
    SymbolistApplication() {}

    const String getApplicationName() override       { return ProjectInfo::projectName; }
    const String getApplicationVersion() override    { return ProjectInfo::versionString; }
    bool moreThanOneInstanceAllowed() override       { return true; }

    //==============================================================================
    void initialise (const String& commandLine) override
    {
        
        /*
        symbolist_handler = SymbolistHandler::symbolistAPI_newSymbolist();
        symbolist_handler->inStandalone();
        symbolist_handler->symbolistAPI_openWindow();
         */
        
        // making the app use the void pointer system for cross checking
        symbolist_handler_ptr = symbolistNew();
        symbolistOpenWindow(symbolist_handler_ptr);
    }

    void shutdown() override
    {
        // mainWindow = nullptr; // (deletes our window)
        // Add your application's shutdown code here..
//        symbolist_handler->symbolistAPI_closeWindow();
  
        symbolistFree( symbolist_handler_ptr );
    }

    //==============================================================================
    void systemRequestedQuit() override
    {
        // This is called when the app is being asked to quit: you can ignore this
        // request and let the app carry on running, or call quit() to allow the app to close.
        quit();
    }

    void anotherInstanceStarted (const String& commandLine) override
    {
        // When another instance of the app is launched while this one is running,
        // this method is invoked, and the commandLine parameter tells you what
        // the other instance's command-line arguments were.
    }


private:
    
    //ScopedPointer<AppMainWindow> mainWindow;
//    ScopedPointer<SymbolistHandler> symbolist_handler;
    void* symbolist_handler_ptr;
};

//==============================================================================
// This macro generates the main() routine that launches the app.
START_JUCE_APPLICATION (SymbolistApplication)
