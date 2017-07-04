#include "SymbolPropertiesPanel.h"


SymbolPropertiesPanel::SymbolPropertiesPanel(SymbolistHandler *sh )
{
    setOpaque (true);
    addAndMakeVisible (symbol_inspector);
    setSize (400, 600);
    
    symbolist_handler = sh;
    change_callback_fn = std::bind( &SymbolPropertiesPanel::change_callback, this, std::placeholders::_1);
    
}

void SymbolPropertiesPanel::change_callback( const OSCMessage& msg)
{
    Symbol *s = symbol_component->getScoreSymbolPointer();
    const OSCBundle *bundle = s->getOSCBundle();
    
    OSCBundle newBundle;
    
    for( auto osc : *bundle )
    {
        if( osc.getMessage().getAddressPattern() == msg.getAddressPattern() )
            newBundle.addElement( msg );
        else
            newBundle.addElement( osc );
    }
    
    s->setOSCBundle( &newBundle );
    
    symbolist_handler->updateSymbolFromInspector( symbol_component );

    s->printBundle();
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
            else if( addr == "/objectType" ) // << this should be something like: /objectType : "staff"
            {
                StringArray choices = {"object", "staff"};

                properties.add( new OSCOptionMenu ( "/objectType", msg, change_callback_fn, choices ) );
            }
            else if( addr == "/staff" )
            {
                properties.add( new OSCTextProperty( msg.getAddressPattern().toString(), msg, change_callback_fn) );
            }
            else if( msg[0].isString() )
            {
                properties.add( new OSCTextProperty( msg.getAddressPattern().toString(), msg, change_callback_fn) );
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


