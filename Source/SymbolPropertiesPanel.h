
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"
#include "BaseComponent.h"
#include "OSCPropertyComponents.h"

//==============================================================================

typedef std::function<void(const OSCMessage&)> osc_callback_t;


class SymbolPropertiesPanel : public Component
{
    BaseComponent*              symbol_component = nullptr;
    PropertyPanel               symbol_inspector;
    Array<PropertyComponent*>   properties;
    
    osc_callback_t              change_callback_fn;
    
    SymbolistHandler*           symbolist_handler;
    
    int title_offset = 25;
    
public:
    SymbolPropertiesPanel(SymbolistHandler *sh );
    
    void paint (Graphics& g) override;
    void resized() override;
    
    SymbolistHandler* getSymbolistHandler(){ return symbolist_handler; }
    
    void clearInspector();
    
    void setInspectorObject( BaseComponent *c );
    void createOSCview ();
    void updateBundle();

    void change_callback(const OSCMessage& msg);
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolPropertiesPanel)
};
