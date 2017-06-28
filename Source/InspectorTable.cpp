
#include "InspectorTable.h"
#include "SymbolistHandler.h"


OSCInspectorTable::OSCInspectorTable( SymbolistHandler *sh ) : font (14.0f)
{
    symbolist_handler = sh;

    setComponentID("InspectorComponent");
    addAndMakeVisible (table);
    setSize (400, 600);
    setWantsKeyboardFocus(true);
    addKeyListener(this);
    
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
//    table.getHeader().setSortColumnId (1, true); // sort forwards by the ID column
    
    // un-comment this line to have a go of stretch-to-fit mode
//    table.getHeader().setStretchToFitActive (true);
    
    table.setMultipleSelectionEnabled (true);
}


bool OSCInspectorTable::keyPressed (const KeyPress& key, Component* originatingComponent)
{
    if( key == KeyPress ('i', ModifierKeys::commandModifier, 0) ){ symbolist_handler->symbolistAPI_toggleInspectorWindow(); }
    
    return true;
}



void OSCInspectorTable::setInspectorObject( BaseComponent* c )
{
 //   cout << "addSymbolData" << endl;

    symbol_component = c;
    
//    for (auto osc : data )
//        bundle.addElement( osc );

    table.updateContent();
    table.repaint();
}

int OSCInspectorTable::getNumRows()
{
    if( symbol_component != nullptr && symbol_component->getScoreSymbolPointer() != nullptr && symbol_component->getScoreSymbolPointer()->getOSCBundle() != nullptr )
    {
        return symbol_component->getScoreSymbolPointer()->getOSCBundle()->size();
    }
    else
        return 0;
}

// This is overloaded from TableListBoxModel, and must paint any cells that aren't using custom
// components.
void OSCInspectorTable::paintCell (Graphics& g, int rowNumber, int columnId,
                int width, int height, bool /*rowIsSelected*/)
{
    if( symbol_component == nullptr ) return;
    
    const OSCBundle bundle = *(symbol_component->getScoreSymbolPointer()->getOSCBundle());
    
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


    if (columnId == 1 ) // The ID and Length columns do not have a custom component
    {
        jassert (existingComponentToUpdate == nullptr);
        return nullptr;
    }
    
    EditableTextCustomComponent* textLabel = static_cast<EditableTextCustomComponent*> (existingComponentToUpdate);
    
    // same as above...
    if (textLabel == nullptr)
        textLabel = new EditableTextCustomComponent (*this);
    
    textLabel->setRowAndColumn (rowNumber, columnId);
    return textLabel;

    
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
   
    if( symbol_component == nullptr ) return String();
    
    OSCBundle bundle = *(symbol_component->getScoreSymbolPointer()->getOSCBundle());

    switch( columnNumber )
    {
        case 1:
            return (String)rowNumber;
        case 2:
            return bundle[ rowNumber ].getMessage().getAddressPattern().toString();
        case 3:
        {
            String text;
            auto msg = bundle[ rowNumber ].getMessage();
            
            for( int i = 0; i < msg.size(); i++ )
            {
                auto val = msg[i];
                if( val.isFloat32() )
                    text += (String)val.getFloat32() + " ";
                else if( val.isInt32() )
                    text += (String)val.getInt32() + " ";
                else if( val.isString() )
                    text += val.getString() + " ";
                else if( val.isBlob() )
                    text += (String)val.getBlob().toString() + " ";
            }
            return text;
        }
            
        default:
            return (String)"???";
    }
   
    return "getText failed";
}

void OSCInspectorTable::setText (const int columnNumber, const int rowNumber, const String& newText)
{
    if( symbol_component == nullptr ) return;
    OSCBundle bundle = *(symbol_component->getScoreSymbolPointer()->getOSCBundle());
    
    switch( columnNumber )
    {
        case 1:
            cout << "would be cool to set a mapping id here" << endl;
            break;
        case 2:
        {
            cout << "not allowed to change address! sorry " << endl;
            break;
        }
        case 3:
        {
            String text;

            OSCMessage msg = bundle[ rowNumber ].getMessage();
            OSCAddressPattern addr = msg.getAddressPattern();
            OSCBundle new_bndl;
            
            for( auto osc : bundle )
            {
                if( osc.getMessage().getAddressPattern() != addr )
                {
                    new_bndl.addElement(osc);
                }
                else
                {
                    
                    
                    StringArray tokens;
                    tokens.addTokens(newText, " ");
                    tokens.removeEmptyStrings();
                    
                    if(  msg.size() == 0 || ( tokens.size() != msg.size() ) )
                        return;
                   
                    
                    OSCMessage new_mess(addr);
                    for( int i = 0; i < msg.size(); i++ )
                    {
                        auto val = msg[i];
                        if( val.isFloat32() )
                            new_mess.addArgument( OSCArgument(tokens[i].getFloatValue()) );
                        else if( val.isInt32() )
                            new_mess.addArgument( OSCArgument(tokens[i].getIntValue()) );
                        else if( val.isString() )
                            new_mess.addArgument( OSCArgument(tokens[i]) );
                        else if( val.isBlob() )
                            new_mess.addArgument( OSCArgument("not sure how to deal with blobs") );
                    }
                    
                    if( new_mess.size() > 0 )
                        new_bndl.addElement( new_mess );
                    
                }
            }
            
            Symbol s;
            s.setOSCBundle(&new_bndl);
            symbolist_handler->updateSymbolFromInspector( symbol_component, s );
            s.printBundle();
            break;
        }
            
        default:
            cout << "unhandled column " << columnNumber << endl;
            break;
    }
    
}

void OSCInspectorTable::resized()
{
    // position our table with a gap around its edge
    table.setBoundsInset (BorderSize<int> (8));
}

void OSCInspectorTable::paintRowBackground (Graphics& g, int rowNumber, int /*width*/, int /*height*/, bool rowIsSelected)
{
    const Colour alternateColour (getLookAndFeel().findColour (ListBox::backgroundColourId)
                                  .interpolatedWith (getLookAndFeel().findColour (ListBox::textColourId), 0.03f));
    if (rowIsSelected)
        g.fillAll (Colours::lightblue);
    else if (rowNumber % 2)
        g.fillAll (alternateColour);
}

