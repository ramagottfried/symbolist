
#pragma once

#include "SymbolPropertiesPanel.h"
#include "ScoreBundleViewer.h"


class PropertyPanelTabs   : public TabbedComponent
{

public:
    PropertyPanelTabs( SymbolistHandler *sh ) : TabbedComponent(TabbedButtonBar::Orientation::TabsAtTop)
    {
        symbolist_handler = sh;
        symbol_panel_tab = new SymbolPropertiesPanel(sh);
        const Colour c = Colours::lightgrey;
        addTab ("inspector",  c, symbol_panel_tab, false);
        addTab ("score bundle",  c, new ScoreBundleViewPort(sh), true);
        
        setSize (400, 600);

    }
    
    SymbolistHandler* getSymbolistHandler(){ return symbolist_handler; }

    void setInspectorObject( BaseComponent *c ){ symbol_panel_tab->setInspectorObject(c); }
    void createOSCview (){ symbol_panel_tab->createOSCview(); }
    void updateBundle(){ symbol_panel_tab->updateBundle(); }
    void clearInspector(){ symbol_panel_tab->clearInspector(); }
    
private:
    
    SymbolistHandler*                       symbolist_handler;
    ScopedPointer<SymbolPropertiesPanel>    symbol_panel_tab;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PropertyPanelTabs)

};
