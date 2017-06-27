
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"
#include "BaseComponent.h"


class OSCInspectorTable : public Component, public TableListBoxModel, public KeyListener
{
public:
    OSCInspectorTable( SymbolistHandler *sh );
    bool keyPressed (const KeyPress& key, Component* originatingComponent) override;
    
    void setInspectorObject( BaseComponent *c );
    
    int getNumRows() override;

    // This is overloaded from TableListBoxModel, and should fill in the background of the whole row
    void paintRowBackground (Graphics& g, int rowNumber, int /*width*/, int /*height*/, bool rowIsSelected) override;
    

    void paintCell (Graphics& g, int rowNumber, int columnId,
                    int width, int height, bool /*rowIsSelected*/) override;
    
    // This is overloaded from TableListBoxModel, and must update any custom components that we're using
    Component* refreshComponentForCell (int rowNumber, int columnId, bool /*isRowSelected*/,
                                        Component* existingComponentToUpdate) override;

    // This is overloaded from TableListBoxModel, and should choose the best width for the specified
    // column.
    int getColumnAutoSizeWidth (int columnId) override;
   
    String getText (const int columnNumber, const int rowNumber) const;
    
    void setText (const int columnNumber, const int rowNumber, const String& newText);
   
    void resized() override;
    
private:
    // this loads the embedded database XML file into memory
    void loadData( OSCBundle& data );
    
    // (a utility method to search our XML for the attribute that matches a column ID)
    String getAttributeNameForColumnId (const int columnId) const;

    //==============================================================================
    TableListBox        table;
    Font                font;
    
    BaseComponent*      symbol_component = nullptr;
    
    SymbolistHandler*   symbolist_handler;
    
    //==============================================================================
    // This is a custom Label component, which we use for the table's editable text columns.
    class EditableTextCustomComponent  : public Label
    {
    public:
        EditableTextCustomComponent (OSCInspectorTable& td)  : owner (td)
        {
            // double click to edit the label text; single click handled below
            setEditable (false, true, false);
        }
        
        void mouseDown (const MouseEvent& event) override
        {
            // single click on the label should simply select the row
            owner.table.selectRowsBasedOnModifierKeys (row, event.mods, false);
            
            Label::mouseDown (event);
        }
        
        void textWasEdited() override
        {
            owner.setText (columnId, row, getText());
        }
        
        // Our demo code will call this when we may need to update our contents
        void setRowAndColumn (const int newRow, const int newColumn)
        {
            row = newRow;
            columnId = newColumn;
            setText (owner.getText(columnId, row), dontSendNotification);
        }
        
        void paint (Graphics& g) override
        {
            auto& lf = getLookAndFeel();
            if (! dynamic_cast<LookAndFeel_V4*> (&lf))
                lf.setColour (textColourId, Colours::black);
            
            Label::paint (g);
        }
        
    private:
        OSCInspectorTable& owner;
        int row, columnId;
        Colour textColour;
    };
    
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCInspectorTable)
};
