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
        // Instantiates the model.
        SymbolistModel* model = new SymbolistModel();
        
        // Adds four default items to the model.
        float symbol_size = 30.0;
        float symbol_pos = 0.0;
        
        Palette* palette = model->getPalette();
        
        Symbol s1 = Symbol();
        s1.setTypeXYWH("text", symbol_pos, symbol_pos, 20 , 20);
        palette->addDefaultItem(s1);
        
        Symbol s2 = Symbol();
        s2.setTypeXYWH("circle", symbol_pos, symbol_pos, symbol_size, symbol_size);
        palette->addDefaultItem(s2);
        
        Symbol s3 = Symbol();
        s3.setTypeXYWH("rectangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
        palette->addDefaultItem(s3);
        
        Symbol s4 = Symbol();
        s4.setTypeXYWH("triangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
        palette->addDefaultItem(s4);
        
        /* Casts the void pointer returned by the symbolistNew.
         * symbolistNew() returns a void pointer for cross system
         * compatibility.
         */
        symbolist_handler_ptr = static_cast<SymbolistHandler*>(symbolistNew());
        symbolist_handler_ptr->setModel(model);
        
        // Adds the symbolist_handler_ptr as an observer of the model.
        model->attach(symbolist_handler_ptr);
        
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
