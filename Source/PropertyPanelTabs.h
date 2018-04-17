
#pragma once

#include "SymbolPropertiesPanel.h"
#include "ScoreBundleViewer.h"


class PropertyPanelTabs : public TabbedComponent {
	
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
        symbol_panel_bundleviewer = new ScoreBundleViewer(sh);
        addTab ("score bundle",  c, symbol_panel_bundleviewer, false);
        
        setSize (400, 600);

    }
    
    void resized() override
    {
        TabbedComponent::resized();
        if( symbol_panel_bundleviewer )
            symbol_panel_bundleviewer->resized();
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
	
private:
	SymbolistHandler*                    symbolist_handler;
    ScopedPointer<SymbolPropertiesPanel> symbol_panel_tab;
    ScopedPointer<ScoreBundleViewer>     symbol_panel_bundleviewer;
	
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PropertyPanelTabs)

};
