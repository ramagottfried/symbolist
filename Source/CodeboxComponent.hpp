#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"


// unfinished and unused at the moment, should basically work, was being used for bundle view, but removed in favor of TreeView

class Codebox : public Component
{
public:
    Codebox(SymbolistHandler *sh) : symbolist_handler(sh) //bundle_comp(sh)
    {
        addAndMakeVisible( bundle_editor = new CodeEditorComponent (bundle_doc, nullptr) );
        bundle_editor->setReadOnly( true );
        bundle_editor->setLineNumbersShown( false );
        loadBundle();

        setSize (400, 600);
        
    }
    
    ~Codebox()
    {
        //resultsTree.setRootItem (nullptr);
    }
	
    void loadBundle()
    {
        
        int num = symbolist_handler->symbolistAPI_getNumSymbols();
        //string prefix = "/symbol/";
        //cout << "getting n " << num << endl;
        
        String content;
        
        for( int i = 0; i < num; i++ )
        {
            Symbol* sym = symbolist_handler->symbolistAPI_getSymbol(i);
            
            vector<string> msg_array;
            sym->getPrintStringArray( msg_array );
            
            for( int j = 0; j < msg_array.size(); j++ )
            {
                //cout << msg_array[j] << endl;
                content += msg_array[j] + "\n";
            }
        }
        
        bundle_editor->loadContent( content );
    }

    void resized() override
    {
        // cout << "osc viewport " << endl;

        loadBundle();
        Rectangle<int> r (getLocalBounds().reduced (8));
        bundle_editor->setBounds (r.withTrimmedTop (8));
    }
    
private:
    SymbolistHandler*                   symbolist_handler;
    
    CodeDocument                        bundle_doc;
    ScopedPointer<CodeEditorComponent>  bundle_editor;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Codebox)
};






