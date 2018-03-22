#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "BaseComponent.h"
#include "SymbolistComponent.h"
#include "Score.h"


/**
 * Describes an element of the palette.
 */
class PaletteButton : public SymbolistComponent
{

public:
    
    PaletteButton(int i, Symbol *s);
    ~PaletteButton();
    
    void setSelected(bool sel);
    
    void resized() override;
    void paint(Graphics &g) override;
    void mouseDown ( const MouseEvent& event ) override;
 
    int getID() { return button_id; }
    
private:
    
    ScopedPointer<BaseComponent> graphic_comp;
    int button_id;
    bool selected;
    
};


/**
 * Describes the graphic component representing the palette.
 */
class PaletteComponent : public SymbolistComponent
{
    
    public:
    
    PaletteComponent();
    ~PaletteComponent();
    
    void buildFromPalette(Palette *palette);
    void selectPaletteButton(int i);
    
    void paint (Graphics& g) override;
    
    private:
    
    Palette* palette_pointer;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PaletteComponent)

};
