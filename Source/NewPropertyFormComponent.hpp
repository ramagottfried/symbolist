#ifndef NewPropertyFormComponent_hpp
#define NewPropertyFormComponent_hpp

#include <stdio.h>
#include <algorithm>
#include "JuceHeader.h"
#include "symbolist-utils.hpp"

/**
 * Describes the form displayed when the '+' button in the inspector is clicked.
 * When the form is submitted, a new odot message is added to the inspected symbol.
 *
 */
class NewPropertyFormComponent : public virtual Component, public virtual Button::Listener {

public:
	NewPropertyFormComponent();
	void resized() override;
	
	
	void buttonClicked(Button* button) override;
	
private:
	TextButton 	submit_button;
	
	/** A label holding the new odot message address.
	 *  Should be constrained to begin with '/'
	 */
	Label 		property_name_label;
	
	/** An array of strings holding the possible types
	 *  of an odot message.
	 */
	StringArray property_types;
	
	/** A combobox displaying the possible types
	 *  of an odot message.
	 */
	ComboBox	property_types_box;
	
	JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(NewPropertyFormComponent)

};

#endif /* NewPropertyFormComponent_hpp */
