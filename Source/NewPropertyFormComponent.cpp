#include "NewPropertyFormComponent.hpp"
#include "SymbolPropertiesPanel.h"

NewPropertyFormComponent::NewPropertyFormComponent()
{
	/* Sets the property name label */
	property_name_label.setText("property name", NotificationType::dontSendNotification);
	property_name_label.setEditable(true);
	property_name_label.setColour(Label::textColourId, Colours::black);
	property_name_label.setColour (Label::backgroundColourId, Colours::lightgrey);
	addAndMakeVisible(property_name_label);
	
	/* Sets the property types combobox and the associated item list */
	property_types.add("int");
	property_types.add("float");
	property_types.add("string");

	property_types_box.setText("property type");
	property_types_box.addItemList(property_types, 1);
	addAndMakeVisible(property_types_box);
	
	/* Sets the confirmation button, used to submit the property creation */
	submit_button.setButtonText("OK");
	submit_button.addListener(this);
	addAndMakeVisible(submit_button);
	
}

void NewPropertyFormComponent::resized()
{
	auto area = getLocalBounds();
	property_name_label.setBounds(area.removeFromLeft(getWidth() * 0.45));
	property_types_box .setBounds(area.removeFromLeft(getWidth() * 0.35));
	submit_button	   .setBounds(area.removeFromLeft(getWidth() * 0.2));
}

void NewPropertyFormComponent::buttonClicked(Button* button)
{
	SymbolPropertiesPanel* symbolPropertiesPanel = dynamic_cast<SymbolPropertiesPanel*>(getParentComponent());
	if (symbolPropertiesPanel != NULL)
	{
		String propertyName = property_name_label.getText();
		String propertyType = property_types_box.getText();
//
//		if (!propertyName.startsWith("/"))
//		{
//			// display message, NYI.
//		}
//		else if (propertyType == "property type")
//		{
//			// display message, NYI.
//		}
		
		symbolPropertiesPanel->addMessageToInspectedSymbol(propertyName, propertyType);
		
	}
	else throw logic_error("NewPropertyFormComponent has no parent of type SymbolPropertiesPanel");
}
