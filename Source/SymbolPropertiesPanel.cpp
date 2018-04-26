#include "SymbolPropertiesPanel.h"
#include "OSCPropertyComponents.h"
#include "InspectorComponent.h"

SymbolPropertiesPanel::SymbolPropertiesPanel(SymbolistHandler *sh )
{
    setOpaque (false);
    addAndMakeVisible (symbol_inspector);
    setSize (200, 600);
    
    symbolist_handler = sh;
    change_callback_fn = std::bind( &SymbolPropertiesPanel::change_callback, this, std::placeholders::_1);
    
    getLookAndFeel().setColour( PropertyComponent::backgroundColourId, Colours::transparentWhite );
    getLookAndFeel().setColour( PropertyComponent::labelTextColourId, Colours::black );
    
    getLookAndFeel().setColour(Label::textWhenEditingColourId, Colours::black  );
    getLookAndFeel().setColour(Label::backgroundWhenEditingColourId, Colours::transparentWhite  );
}

void SymbolPropertiesPanel::change_callback( const OdotMessage& msg)
{
    Symbol* s = symbol_component->getScoreSymbol();
	
    if (s != NULL)
    {
    	s->addMessage( msg ); // this overwrites any duplicate messages
		InspectorComponent* inspectorView = dynamic_cast<InspectorComponent* >(getParentComponent());
		
    	inspectorView->updateSymbolFromComponent( symbol_component );
    }
    // DEBUG_INLINE("******************** updated bundle from inspector ********** " << endl)
    
}

void SymbolPropertiesPanel::createOSCview ()
{
    properties.clear();

    if( symbol_component )
    {
        Symbol* s = symbol_component->getScoreSymbol();
        // DEBUG_FULL("Retrieving pointer from component" << endl)
        // s->print();
        
        if( !s )
            return;
        
        if ( !s->size() )  return;

        
        for ( auto msg : s->getMessageArray() )
        {
            const string& addr = msg.getAddress();
            
            // skipping subbundles for now! need to fix this
            
            
            if( addr == "/color" )  // should have separate selection for stroke color and the other stroke parameters
            {
                properties.add( new OSCColourSelectorButton(addr, msg, change_callback_fn) );
            }
            else if( addr == "/font" )
            {
                properties.add( new OSCFontMenu ( addr, msg, change_callback_fn, Font::findAllTypefaceNames() ) );
            }
            else if( addr == "/staff" )
            {
                if( s->getMessage("/type").getString() != "staff" )
                {
                    StringArray staves = symbolist_handler->getStaves();
                    staves.insert(0, String("<none>") );
                    
                    properties.add( new OSCOptionMenu ( addr, msg, change_callback_fn, staves ) );
                }
            }
            else if( addr == "/fill" )
            {
                properties.add( new OSCOnOffButton( addr, msg, change_callback_fn) );
            }
            else if( addr == "/type" )
            {
                properties.add( new OSCValueDisplay( addr, msg) );
            }
            else if( addr == "/id" )
            {
                properties.add( new OSCValueDisplay( addr, msg) );
            }
            else if( addr == "/num_sub_paths" )
            {
                properties.add( new OSCValueDisplay( addr, msg) );
            }
            else if( addr == "/length" )
            {
                properties.add( new OSCValueDisplay( addr, msg) );
            }
           /* else if( addr == "lambda" )
            {
                properties.add( new OSCTextProperty( addr, msg, change_callback_fn, true ) );
            }*/
            else if( msg[0].isString() )
            {
                properties.add( new OSCTextProperty( addr, msg, change_callback_fn) );
            }
            else if( msg[0].isFloat() )
            {
                properties.add( new OSCFloatValueSlider( addr, msg, change_callback_fn ) );
            }
            else if( msg[0].isInt() )
            {
                properties.add( new OSCIntValueSlider( addr, msg, change_callback_fn ) );
            }
        }
    
    }
    symbol_inspector.addProperties( properties );
//    symbol_inspector.addSection("OSC", properties);
}

void SymbolPropertiesPanel::setInspectorObject( BaseComponent *c )
{
    symbol_component = c;
    symbol_inspector.clear();
    createOSCview();
}

void SymbolPropertiesPanel::clearInspector()
{
    if( !symbol_inspector.isEmpty() )
        symbol_inspector.clear();
    
    symbol_component = nullptr;
}

void SymbolPropertiesPanel::paint (Graphics& g)
{
    // background
    g.fillAll ( Colour::fromFloatRGBA(0.95, 0.95, 0.95, 0.7) );
    
    // left separator line
    g.setColour( Colours::lightgrey );
    g.drawLine( 0, 0, 0, getHeight() );
    
    g.fillRect(0, 0, getWidth(), title_offset) ;
    
    g.drawLine( 0, title_offset, getWidth(), title_offset);
    
    // title
    g.setColour( Colours::white );
    auto f = g.getCurrentFont();
    f.setItalic(true);
    g.setFont( f );
    g.drawText("object inspector", 4, 0, getWidth()-8, title_offset, Justification::left);
}

void SymbolPropertiesPanel::resized() 
{
    auto local = getLocalBounds().reduced(4);
    symbol_inspector.setSize(local.getWidth(), local.getHeight() - title_offset);
    symbol_inspector.setTopLeftPosition(local.getX(), local.getY() + title_offset);
}

