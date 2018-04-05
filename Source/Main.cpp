#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistModel.hpp"
#include "SymbolistHandler.h"
#include "symbolist.hpp"


//==============================================================================
class SymbolistApplication  : public JUCEApplication
{
 
    SymbolistHandler* symbolist_handler_ptr;

public:
    //==============================================================================
    SymbolistApplication() {}

    const String getApplicationName() override       { return ProjectInfo::projectName; }
    const String getApplicationVersion() override    { return ProjectInfo::versionString; }
    bool moreThanOneInstanceAllowed() override       { return true; }

    //==============================================================================
    void initialise (const String& commandLine) override
    {
        
        /* Casts the void pointer returned by the symbolistNew.
         * symbolistNew() returns a void pointer for cross system
         * compatibility.
         */
        symbolist_handler_ptr = static_cast<SymbolistHandler*>(symbolistNew());
        symbolistOpenWindow(symbolist_handler_ptr);
    }

    void shutdown() override
    {
        // mainWindow = nullptr; // (deletes our window)
        // Add your application's shutdown code here..
        // symbolist_handler->symbolistAPI_closeWindow();
  
        symbolistFree(symbolist_handler_ptr);
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

};

//==============================================================================
// This macro generates the main() routine that launches the app.
START_JUCE_APPLICATION (SymbolistApplication)
