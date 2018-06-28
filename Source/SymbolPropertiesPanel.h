
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"
#include "BaseComponent.h"
#include "symbolist-utils.hpp"
#include "NewPropertyFormComponent.hpp"

//==============================================================================

class InspectorComponent;
typedef std::function<void(const OdotMessage&)> osc_callback_t;

class SymbolPropertiesPanel : public Component, public Button::Listener {

public:
	/************************************
	 *            CONSTRUCTORS          *
	 ************************************/
    SymbolPropertiesPanel();
    ~SymbolPropertiesPanel();

	/************************************
	 *         GETTERS & SETTERS        *
	 ************************************/
	inline PropertyPanel* getSymbolInspector() { return &symbol_inspector; }
	float getPreferedHeight();
	
    void paint (Graphics& g) override;
    void resized() override;
	
    void clearInspector();
    
    void setInspectorObject( BaseComponent *c );
    void createOSCview();
    void updateBundle();

    void change_callback(const OdotMessage& msg);
	void buttonClicked(Button* button) override;
	
	/**
	 * Gets the InspectorController instance from the InspectorComponent
	 * and calls the InspectorController::addMessageToInspectedSymbol passing
	 * the symbol associated with the inspected graphic component in parameter.
	 *
	 * @throw logic_error If this instance of SymbolPropertiesPanel has no parent
	 *					  of type InspectorComponent.
	 *
	 * @throw logic_error If this instance of SymbolPropertiesPanel has a parent
	 *					  of type InspectorComponent, but its parent is not associated
	 *					  with any controller.
	 *
	 * @throw logic_error If the inspected graphic component is not associated with any symbol.
	 *
	 * @see   InspectorController#addMessageToInspectedSymbol(Symbol*, String, String)
	 *		  InspectorController::addMessageToInspectedSymbol(Symbol*, String, String)
	 */
	void addMessageToInspectedSymbol(String messageAddress, String messageValue);
	
	/**
	 * Wrapper method around the InspectorComponent::toggleCodeBox method.
	 *
	 * @throws logic_error If this SymbolPropertiesPanel has no parent of
	 *					   type InspectorComponent.
	 *
	 * @see    InspectorComponent#toggleCodeBox() InspectorComponent::toggleCodeBox()
	 */
	void toggleCodeBox();
	
private:
	BaseComponent*              symbol_component;
    PropertyPanel               symbol_inspector;
    Array<PropertyComponent* >  properties;
	osc_callback_t              change_callback_fn;
	
    /************************************
	 *        ADD PROPERTY FEATURE      *
	 ************************************/
	TextButton 					add_property_button;
	NewPropertyFormComponent    add_property_form;
	
	/***************************************
	 *        EVALUATE BUNDLE FEATURE      *
	 ***************************************/
	TextButton evaluate_bundle_button;
	
	/*********************************
	 *        LAYOUT PROPERTIES      *
	 *********************************/
    int title_offset = 25;
	int add_property_button_top_margin = 10;
	
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolPropertiesPanel)
};
