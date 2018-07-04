
#pragma once

#include "SymbolPropertiesPanel.h"
#include "ScoreBundleViewer.h"
#include "SymbolistModel.hpp"
#include "InspectorController.hpp"
#include "View.hpp"

/**
 * Describes the inspector view of the symbolist application.
 * An InspectorComponent instance is composed of to tabs:
 * 	- the inspector tab, where each OSC messages contained in the currently selected symbol are shown.
 * 	- the score bundle tab, presenting all symbols of the score in a JSON format.
 */
class InspectorComponent : public virtual TabbedComponent,
						   public virtual View<SymbolistModel, InspectorController >
{
	
public:
    /**************************************************
     *                CONSTRUCTORS                    *
     **************************************************/
    InspectorComponent();
    
    /**************************************************
     *                GETTERS AND SETTERS             *
     **************************************************/
    inline SymbolPropertiesPanel* getSymbolPanelTab() { return symbol_panel_tab.get(); }
    
    /**
     * Gets the minimum height value for this InspectorComponent
     * permitting to display all of its content.
     *
     * @returns The minimum height to show all the elements contained
     *			in this InspectorComponent.
     */
    float getPreferedHeight();
    
	/**************************************************
     *        SYMBOL PROPERTIES PANEL METHODS         *
     **************************************************/
    inline void setInspectorObject( BaseComponent *c ) { symbol_panel_tab->setInspectorObject(c); }
    inline void createOSCview () { symbol_panel_tab->createOSCview(); }
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
	
	/**
	 * Wrapper method around the SymbolistMainComponent::toggleCodeBox
	 * method.
	 *
	 * @throws logic_error If this instance of InspectorComponent has no
	 *                     parent of type SymbolistMainComponent.
	 *
	 * @see SymbolistMainComponent#toggleCodeBox()
	 *		SymbolistMainComponent::toggleCodeBox
	 */
	void toggleCodeBox();
	
	/* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}
    
private:

    ScopedPointer<SymbolPropertiesPanel > symbol_panel_tab;
    ScopedPointer<ScoreBundleViewer >     symbol_panel_bundleviewer;
	
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (InspectorComponent)

};
