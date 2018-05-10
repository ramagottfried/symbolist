#pragma once

#ifndef PaletteButton_hpp
#define PaletteButton_hpp

#include <stdio.h>
#include "../JuceLibraryCode/JuceHeader.h"
#include "BaseComponent.h"
#include "Score.h"

/**
 * Describes an element of the palette.
 */
class PaletteButton : public SymbolistComponent
{
	
public:
    PaletteButton(int i, Symbol* s);
    inline ~PaletteButton() {};
    
    void setSelected(bool sel);
    
    void resized() override;
    void paint(Graphics &g) override;
    void mouseDown(const MouseEvent& event) override;
    
    int getID() { return button_id; }
	
private:
	ScopedPointer<BaseComponent > graphic_comp;
    int button_id;
    bool selected;
};

#endif /* PaletteButton_hpp */
