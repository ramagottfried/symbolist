
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"


class OSCInspectorTable : public Component, public TableListBoxModel
{
public:
    OSCInspectorTable( SymbolistHandler *sh );
    
    void setInspectorData( OSCBundle data );
    
    int getNumRows() override { return bundle.size(); }

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
    TableListBox    table;
    Font            font;
    
    OSCBundle       bundle;
    
    int             numRows;            // The number of rows of data we've got
    
    SymbolistHandler*   symbolist_handler;    
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCInspectorTable)
};