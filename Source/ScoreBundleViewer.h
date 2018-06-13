#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"
#include "JsonTreeItem.hpp"


/* note:
 Juce has an example of how to create a treeview display for JSON
 and so I made a JSON export and now have the JSON viewer working, but I think we might be able to convert it to use OSC as well
 
 we could subclass the TreeViewItem class, like the OSCProperty classes and then have a tree view of editable properties.
 
 */
class ScoreBundleViewer : public Component {

public:
    ScoreBundleViewer()
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
	
	inline float getPreferedHeight() { return rootItem->getItemHeight(); } 
	
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
    
    TreeViewItem* rebuildJson();
	
	void paint(Graphics& g) override
	{
		createNewRootNode();
	}
	
    void resized() override
    {
        Rectangle<int> area (getLocalBounds());
        resultsTree.setBounds (area.reduced (8));
    }
    
private:
    ScopedPointer<TreeViewItem>         rootItem;
    TreeView                            resultsTree;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreBundleViewer)
};





