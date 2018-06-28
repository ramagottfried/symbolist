#include "CallbackButtonPropertyComponent.hpp"


CallbackButtonPropertyComponent::CallbackButtonPropertyComponent(const String& propertyName, bool triggerOnMouseDown, function<void()> onClickCallback) :
ButtonPropertyComponent(propertyName, triggerOnMouseDown), on_click_callback(onClickCallback)
{
	
}

void CallbackButtonPropertyComponent::buttonClicked()
{
	on_click_callback();
}

String CallbackButtonPropertyComponent::getButtonText() const
{
	return "Show code box";
}
