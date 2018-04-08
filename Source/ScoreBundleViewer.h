#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"
#include "JsonTreeHandler.hpp"

class ScoreBundleViewer : public Component
{
public:
    ScoreBundleViewer(SymbolistHandler *sh) : symbolist_handler(sh) //bundle_comp(sh)
    {
        addAndMakeVisible (resultsTree);
        resultsTree.setColour (TreeView::backgroundColourId, Colours::white);
        resultsTree.setDefaultOpenness (true);
        
        /*
        addAndMakeVisible( bundle_editor = new CodeEditorComponent (bundle_doc, nullptr) );
        bundle_editor->setReadOnly( true );
        bundle_editor->setLineNumbersShown( false );
        loadBundle();
        */
        rebuildTree();
        setSize (400, 600);

    }
    
    ~ScoreBundleViewer()
    {
        resultsTree.setRootItem (nullptr);
    }
    
    /*
    void loadBundle()
    {
        
        int num = symbolist_handler->symbolistAPI_getNumSymbols();
        //string prefix = "/symbol/";
        //cout << "getting n " << num << endl;
        
        String content;
        
        for( int i = 0; i < num; i++ )
        {
            Symbol* sym = symbolist_handler->symbolistAPI_getSymbol(i);
            
            cout << sym->getJSON() << endl;
            
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
     */
    
    void rebuildTree()
    {
        ScopedPointer<XmlElement> openness;
        
        if (rootItem != nullptr)
            openness = rootItem->getOpennessState();
        
        createNewRootNode();
        
        if (openness != nullptr && rootItem != nullptr)
            rootItem->restoreOpennessState (*openness);
    }
    
    void createNewRootNode()
    {
        // clear the current tree
        resultsTree.setRootItem (nullptr);
        rootItem = nullptr;
        rootItem = rebuildJson();
        resultsTree.setRootItem (rootItem);
    }
    
    /** Parses the editors contects as JSON. */
    TreeViewItem* rebuildJson()
    {
        var parsedJson;
        //cout << symbolist_handler->getModel()->getScore()->getJSON() << endl;
        Result result = JSON::parse ( symbolist_handler->getModel()->getScore()->getJSON() , parsedJson);
        
        if (! result.wasOk())
        {
            cout << "Error parsing JSON: " + result.getErrorMessage() << endl;
            return nullptr;
        }
        
        return new JsonTreeItem (Identifier(), parsedJson);
    }

    void resized() override
    {
        // cout << "osc viewport " << endl;
        /*
        loadBundle();
        Rectangle<int> r (getLocalBounds().reduced (8));
        bundle_editor->setBounds (r.withTrimmedTop (8));
         */
        createNewRootNode();
        Rectangle<int> area (getLocalBounds());
        resultsTree.setBounds (area.reduced (8));
    }
    
private:
    SymbolistHandler*                   symbolist_handler;

    ScopedPointer<TreeViewItem>         rootItem;
    TreeView resultsTree;

    /*
    CodeDocument                        bundle_doc;
    ScopedPointer<CodeEditorComponent>  bundle_editor;
    */
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreBundleViewer)
};





