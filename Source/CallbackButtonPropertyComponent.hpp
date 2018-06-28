//
//  CallbackButtonPropertyComponent.hpp
//  symbolist
//
//  Created by Vincent Iampietro on 18/06/2018.
//

#ifndef CallbackButtonPropertyComponent_hpp
#define CallbackButtonPropertyComponent_hpp

#include <stdio.h>
#include "symbolist-utils.hpp"

/**
 * Specializes the ButtonPropertyComponent class to receive a callback function
 * at the object creation.
 * The callback function is called when the button displayed by a CallbackButtonPropertyComponent
 * is clicked.
 *
 */
class CallbackButtonPropertyComponent : public ButtonPropertyComponent
{

public:
	CallbackButtonPropertyComponent(const String& propertyName, bool triggerOnMouseDown, function<void()> onClickCallback);
	void buttonClicked() override;
	String getButtonText() const override;
	
private:
	function<void()> on_click_callback;
};

#endif /* CallbackButtonPropertyComponent_hpp */
