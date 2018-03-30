
#pragma once

#include "SymbolPropertiesPanel.h"
#include "ScoreBundleViewer.h"


class PropertyPanelTabs   : public TabbedComponent
{
    
    SymbolistHandler*                       symbolist_handler;
    ScopedPointer<SymbolPropertiesPanel>    symbol_panel_tab;

public:
    /**************************************************
     *                CONSTRUCTORS                    *
     **************************************************/
    inline PropertyPanelTabs( SymbolistHandler *sh ) : TabbedComponent(TabbedButtonBar::Orientation::TabsAtTop)
    {
        symbolist_handler = sh;
        symbol_panel_tab = new SymbolPropertiesPanel(sh);
        const Colour c = Colours::lightgrey;
        addTab ("inspector",  c, symbol_panel_tab, false);
        addTab ("score bundle",  c, new ScoreBundleViewPort(sh), true);
        
        setSize (400, 600);

    }
    
    /**************************************************
     *                GETTERS AND SETTERS             *
     **************************************************/
    inline SymbolistHandler* getSymbolistHandler(){ return symbolist_handler; }
    inline ScopedPointer<SymbolPropertiesPanel> getSymbolPanelTab()
    {
        return symbol_panel_tab;
    }
    
    inline void setInspectorObject( BaseComponent *c ){ symbol_panel_tab->setInspectorObject(c); }
    inline void createOSCview (){ symbol_panel_tab->createOSCview(); }
    inline void updateBundle(){ symbol_panel_tab->updateBundle(); }
    inline void clearInspector(){ symbol_panel_tab->clearInspector(); }
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PropertyPanelTabs)

};
