
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

// Adapted from JUCE Demo TreeVeiw

//==============================================================================
class JsonTreeItem  : public TreeViewItem
{
public:
    JsonTreeItem (Identifier i, var value)   : identifier (i), json (value)
    {
    }
    
    String getUniqueName() const override
    {
        return identifier.toString() + "_id";
    }
    
    bool mightContainSubItems() override
    {
        if (DynamicObject* obj = json.getDynamicObject())
            return obj->getProperties().size() > 0;
        
        return json.isArray();
    }
    
    void paintItem (Graphics& g, int width, int height) override
    {
        // if this item is selected, fill it with a background colour..
        if (isSelected())
            g.fillAll (Colours::blue.withAlpha (0.3f));
        
        g.setColour (Colours::black);
        g.setFont (height * 0.7f);
        
        // draw the element's tag name..
        g.drawText (getText(),
                    4, 0, width - 4, height,
                    Justification::centredLeft, true);
    }
    
    void itemOpennessChanged (bool isNowOpen) override
    {
        if (isNowOpen)
        {
            // if we've not already done so, we'll now add the tree's sub-items. You could
            // also choose to delete the existing ones and refresh them if that's more suitable
            // in your app.
            if (getNumSubItems() == 0)
            {
                // create and add sub-items to this node of the tree, corresponding to
                // the type of object this var represents
                
                if (json.isArray())
                {
                    for (int i = 0; i < json.size(); ++i)
                    {
                        var& child (json[i]);
                        jassert (! child.isVoid());
                        addSubItem (new JsonTreeItem (Identifier(), child));
                    }
                }
                else if (DynamicObject* obj = json.getDynamicObject())
                {
                    NamedValueSet& props (obj->getProperties());
                    
                    for (int i = 0; i < props.size(); ++i)
                    {
                        const Identifier id (props.getName (i));
                        var child (props[id]);
                        jassert (! child.isVoid());
                        addSubItem (new JsonTreeItem (id, child));
                    }
                }
            }
        }
        else
        {
            // in this case, we'll leave any sub-items in the tree when the node gets closed,
            // though you could choose to delete them if that's more appropriate for
            // your application.
        }
    }
    
private:
    Identifier identifier;
    var json;
    
    /** Returns the text to display in the tree.
     This is a little more complex for JSON than XML as nodes can be strings, objects or arrays.
     */
    String getText() const
    {
        String text;
        
        if (identifier.isValid()){
            text << identifier.toString();
        }
        
        if (! json.isVoid())
        {
            if (text.isNotEmpty() && (! json.isArray()))
                text << ": ";
            
            if (json.isObject() && (! identifier.isValid()))
            {
                text << "Score Bundle";
            }
            else if (! json.isArray())
            {
                text << json.toString();
            }
            
        }
        
        return text;
    }
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (JsonTreeItem)
};
