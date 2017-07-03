#include "SymbolPropertiesPanel.h"


SymbolPropertiesPanel::SymbolPropertiesPanel(SymbolistHandler *sh )
{
    setOpaque (true);
    addAndMakeVisible (symbol_inspector);
    setSize (400, 600);
    
    symbolist_handler = sh;
    change_callback_fn = std::bind( &SymbolPropertiesPanel::change_callback, this, std::placeholders::_1);
    
}
/*
static PropertyComponent* createObjectTypeList (OSCMessage& msg)
{
    Array<PropertyComponent*> comps;
    
    StringArray choices;
    Array<var> choiceVars;
    
    choices.add("object");
    choiceVars.add (0);
    
    choices.add("staff");
    choiceVars.add (1);
        
}
*/

void SymbolPropertiesPanel::change_callback( OSCMessage& msg)
{
//    iterate and update OSC value in bundle
    
    cout << "recieved " << msg.getAddressPattern().toString() << endl;
}

void SymbolPropertiesPanel::createOSCview ()
{
    properties.clear();

    if( symbol_component )
    {
        Symbol *s = symbol_component->getScoreSymbolPointer();
        OSCBundle bndl = *s->getOSCBundle();
        for( int i = 0; i < bndl.size(); i++ )
        {
            auto msg = bndl[i].getMessage();
            const String& addr = msg.getAddressPattern().toString();
            
            if( addr == "/color" )
            {
                properties.add( new OSCColourSelectorButton("/color", msg, change_callback_fn) );
            }
            else if( addr == "/staff" ) // << this should be something like: /objectType : "staff"
            {
                StringArray choices = {"object", "staff"};

                properties.add( new OSCOptionMenu ( "/objectType", msg, change_callback_fn, choices ) );
            }
            else if( addr == "/name" )
            {
                ; // text edit
            }
            else if( msg[0].isFloat32() )
            {
                properties.add( new OSCFloatValueSlider( addr, msg, change_callback_fn ) );
            }
        }
    
    }
    
    symbol_inspector.addSection("OSC", properties);
}

void SymbolPropertiesPanel::setInspectorObject( BaseComponent *c )
{
    symbol_component = c;

    createOSCview();
}


