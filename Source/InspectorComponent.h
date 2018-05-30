
#pragma once

#include "SymbolPropertiesPanel.h"
#include "ScoreBundleViewer.h"
#include "SymbolistModel.hpp"
#include "InspectorController.hpp"
#include "View.hpp"

class InspectorComponent : public virtual TabbedComponent,
						   public virtual View<SymbolistModel, InspectorController > {
	
public:
    /**************************************************
     *                CONSTRUCTORS                    *
     **************************************************/
    InspectorComponent();
    
    /**************************************************
     *                GETTERS AND SETTERS             *
     **************************************************/
    inline SymbolPropertiesPanel* getSymbolPanelTab() { return symbol_panel_tab.get(); }
    float getPreferedHeight();
    
    inline void setInspectorObject( BaseComponent *c ) { symbol_panel_tab->setInspectorObject(c); }
    inline void createOSCview () { symbol_panel_tab->createOSCview(); }
    inline void updateBundle() { symbol_panel_tab->updateBundle(); }
    inline void clearInspector() { symbol_panel_tab->clearInspector(); }
	
	/**
	 * Wrapper method around the InspectorController::updateSymbolFromComponent
	 * method.
	 *
	 * @throws logic_error If this instance of InspectorComponent has no
	 *                     attached controller.
	 *
	 * @see InspectorController#updateSymbolFromComponent(BaseComponent*)
	 *		InspectorController::updateSymbolFromComponent
	 */
	void updateSymbolFromComponent(BaseComponent* component);
	
	/* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}
    
private:

    ScopedPointer<SymbolPropertiesPanel > symbol_panel_tab;
    ScopedPointer<ScoreBundleViewer >     symbol_panel_bundleviewer;
	
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (InspectorComponent)

};
