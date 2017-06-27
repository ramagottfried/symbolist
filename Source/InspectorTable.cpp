
#include "InspectorTable.h"



OSCInspectorTable::OSCInspectorTable( SymbolistHandler *sh ) : font (14.0f)
{
    symbolist_handler = sh;

    setComponentID("InspectorComponent");
    addAndMakeVisible (table);
    setSize (400, 600);
    
    table.setModel (this);
    table.setColour (ListBox::outlineColourId, Colours::grey);
    table.setOutlineThickness (1);
    
    
    table.getHeader().addColumn ( "ID",         // name
                                 1,             // id
                                 50,           // width
                                 50,            // min width
                                 100,           // max width
                                 TableHeaderComponent::defaultFlags);
    
    table.getHeader().addColumn ( "Address",    // name
                                 2,             // id
                                 100,           // width
                                 100,            // min width
                                 400,           // max width
                                 TableHeaderComponent::defaultFlags);
    
    table.getHeader().addColumn ( "Value",      // name
                                 3,             // id
                                 225,           // width
                                 50,            // min width
                                 400,           // max width
                                 TableHeaderComponent::defaultFlags);
    
    
    
    // we could now change some initial settings..
    table.getHeader().setSortColumnId (1, true); // sort forwards by the ID column
//        table.getHeader().setColumnVisible (7, false); // hide the "length" column until the user shows it
    
    // un-comment this line to have a go of stretch-to-fit mode
    // table.getHeader().setStretchToFitActive (true);
    
    table.setMultipleSelectionEnabled (true);
}


void OSCInspectorTable::setInspectorData( OSCBundle data )
{
 //   cout << "addSymbolData" << endl;

    bundle = data;
    
//    for (auto osc : data )
//        bundle.addElement( osc );

    table.updateContent();
    table.repaint();
}

// This is overloaded from TableListBoxModel, and must paint any cells that aren't using custom
// components.
void OSCInspectorTable::paintCell (Graphics& g, int rowNumber, int columnId,
                int width, int height, bool /*rowIsSelected*/)
{
    
    cout << "paintCell" << endl;
    
    g.setColour (getLookAndFeel().findColour (ListBox::textColourId));
    g.setFont (font);
    
    switch( columnId )
    {
        case 1:
            g.drawText ( (String)rowNumber, 2, 0, width - 4, height, Justification::centredLeft, true);
            break;
        case 2:
        {
            const String text = bundle[ rowNumber ].getMessage().getAddressPattern().toString();
            g.drawText ( text, 2, 0, width - 4, height, Justification::centredLeft, true);
            break;
        }
        case 3:
        {
            String text;
            auto msg = bundle[ rowNumber ].getMessage();
            
            for( int i = 0; i < msg.size(); i++ )
            {
                auto val = msg[0];
                if( val.isFloat32() )
                    text += (String)val.getFloat32() + " ";
                else if( val.isInt32() )
                    text += (String)val.getInt32() + " ";
                else if( val.isString() )
                    text += val.getString() + " ";
                else if( val.isBlob() )
                    text += (String)val.getBlob().toString() + " ";
            }
            g.drawText ( text, 2, 0, width - 4, height, Justification::centredLeft, true);
            break;
        }

        default:
            cout << "unhandled column " << columnId << endl;
            break;
    }
    
    g.setColour (getLookAndFeel().findColour (ListBox::backgroundColourId));
    g.fillRect (width - 1, 0, 1, height);
}

    /*
// This is overloaded from TableListBoxModel, and tells us that the user has clicked a table header
// to change the sort order.
void sortOrderChanged (int newSortColumnId, bool isForwards) override
{

    if (newSortColumnId != 0)
    {
        DemoDataSorter sorter (getAttributeNameForColumnId (newSortColumnId), isForwards);
        dataList->sortChildElements (sorter);
        
        table.updateContent();
    }
}
     */
    
// This is overloaded from TableListBoxModel, and must update any custom components that we're using
Component* OSCInspectorTable::refreshComponentForCell (int rowNumber, int columnId, bool /*isRowSelected*/,
                                    Component* existingComponentToUpdate)
{
   // cout << "refreshComponentForCell" << endl;

    /*
    if (columnId == 1 || columnId == 7) // The ID and Length columns do not have a custom component
    {
        jassert (existingComponentToUpdate == nullptr);
        return nullptr;
    }
    
    if (columnId == 5) // For the ratings column, we return the custom combobox component
    {
        RatingColumnCustomComponent* ratingsBox = static_cast<RatingColumnCustomComponent*> (existingComponentToUpdate);
        
        // If an existing component is being passed-in for updating, we'll re-use it, but
        // if not, we'll have to create one.
        if (ratingsBox == nullptr)
            ratingsBox = new RatingColumnCustomComponent (*this);
        
        ratingsBox->setRowAndColumn (rowNumber, columnId);
        return ratingsBox;
    }
    
    // The other columns are editable text columns, for which we use the custom Label component
    EditableTextCustomComponent* textLabel = static_cast<EditableTextCustomComponent*> (existingComponentToUpdate);
    
    // same as above...
    if (textLabel == nullptr)
        textLabel = new EditableTextCustomComponent (*this);
    
    textLabel->setRowAndColumn (rowNumber, columnId);
    return textLabel;
     */
    
    return nullptr;
}
    
// This is overloaded from TableListBoxModel, and should choose the best width for the specified
// column.
int OSCInspectorTable::getColumnAutoSizeWidth (int columnId)
{
    cout << "getColumnAutoSizeWidth" << endl;
    int widest = 32;
/*
    // find the widest bit of text in this column..
    for (int i = getNumRows(); --i >= 0;)
    {
        if (const XmlElement* rowElement = dataList->getChildElement (i))
        {
            const String text (rowElement->getStringAttribute (getAttributeNameForColumnId (columnId)));
            
            widest = jmax (widest, font.getStringWidth (text));
        }
    }
 */
    return widest + 8;
}


String OSCInspectorTable::getText (const int columnNumber, const int rowNumber) const
{
   
    return "getText"; //dataList->getChildElement (rowNumber)->getStringAttribute ( getAttributeNameForColumnId(columnNumber));
}

void OSCInspectorTable::setText (const int columnNumber, const int rowNumber, const String& newText)
{
//     const String& columnName = table.getHeader().getColumnName (columnNumber);
    
    // dataList->getChildElement (rowNumber)->setAttribute (columnName, newText);
    
}


//==============================================================================
void OSCInspectorTable::resized()
{
    // position our table with a gap around its edge
    table.setBoundsInset (BorderSize<int> (8));
    repaint();
}


// This is overloaded from TableListBoxModel, and should fill in the background of the whole row
void OSCInspectorTable::paintRowBackground (Graphics& g, int rowNumber, int /*width*/, int /*height*/, bool rowIsSelected)
{
    const Colour alternateColour (getLookAndFeel().findColour (ListBox::backgroundColourId)
                                  .interpolatedWith (getLookAndFeel().findColour (ListBox::textColourId), 0.03f));
    if (rowIsSelected)
        g.fillAll (Colours::lightblue);
    else if (rowNumber % 2)
        g.fillAll (alternateColour);
}

