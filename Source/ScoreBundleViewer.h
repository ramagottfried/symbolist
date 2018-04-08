#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"
#include "JsonTreeItem.hpp"

class ScoreBundleViewer : public Component
{
public:
    ScoreBundleViewer(SymbolistHandler *sh) : symbolist_handler(sh) //bundle_comp(sh)
    {
        addAndMakeVisible (resultsTree);
        resultsTree.setColour (TreeView::backgroundColourId, Colours::white);
        resultsTree.setDefaultOpenness (true);
        
        rebuildTree();
        setSize (400, 600);

    }
    
    ~ScoreBundleViewer()
    {
        resultsTree.setRootItem (nullptr);
    }
    
    
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
        
        createNewRootNode();
        Rectangle<int> area (getLocalBounds());
        resultsTree.setBounds (area.reduced (8));
    }
    
private:
    SymbolistHandler*                   symbolist_handler;

    ScopedPointer<TreeViewItem>         rootItem;
    TreeView resultsTree;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreBundleViewer)
};





