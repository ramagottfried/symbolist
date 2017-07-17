
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"
#include "BaseComponent.h"
#include "OSCPropertyComponents.h"

//==============================================================================

typedef std::function<void(const OSCMessage&)> osc_callback_t;


class SymbolPropertiesPanel   : public Component
{
public:
    SymbolPropertiesPanel(SymbolistHandler *sh );
    
    void paint (Graphics& g) override
    {
        g.fillAll ( Colour::fromFloatRGBA(0, 0, 0, 0.05) );
        g.setColour(Colours::lightgrey);
        g.drawLine( 0, 0, 0, getHeight() );
    }
    
    void resized() override
    {
        symbol_inspector.setBounds (getLocalBounds().reduced (4));
    }
    
    SymbolistHandler* getSymbolistHandler(){ return symbolist_handler; }
    
    void clearInspector()
    {
        if( !symbol_inspector.isEmpty() )
        {
            symbol_inspector.clear();
        }
        
        symbol_component = nullptr;
    }
    
    void setInspectorObject( BaseComponent *c );
    void createOSCview ();
    void updateBundle();

    void change_callback(const OSCMessage& msg);
    
private:
    BaseComponent*              symbol_component = nullptr;
    PropertyPanel               symbol_inspector;
    Array<PropertyComponent*>   properties;
    
    osc_callback_t              change_callback_fn;
    
    SymbolistHandler*           symbolist_handler;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolPropertiesPanel)
};
