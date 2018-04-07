

#pragma once
#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"


class ScoreBundleViewer : public Component
{
public:
    ScoreBundleViewer(SymbolistHandler *sh) : symbolist_handler(sh) //bundle_comp(sh)
    {
        addAndMakeVisible( bundle_editor = new CodeEditorComponent (bundle_doc, nullptr) );
        setSize (400, 600);
        loadBundle();
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
    
    ~ScoreBundleViewer() {}

    void resized() override
    {
        // cout << "osc viewport " << endl;
        loadBundle();
        Rectangle<int> r (getLocalBounds().reduced (8));
        bundle_editor->setBounds (r.withTrimmedTop (8));

    }
    
private:
    SymbolistHandler*                 symbolist_handler;

    CodeDocument                        bundle_doc;
    ScopedPointer<CodeEditorComponent>  bundle_editor;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreBundleViewer)
};



