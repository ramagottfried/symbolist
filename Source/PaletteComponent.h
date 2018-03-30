#pragma once

#ifndef PaletteComponent_hpp
#define PaletteComponent_hpp

#include "../JuceLibraryCode/JuceHeader.h"
#include "BaseComponent.h"
#include "Score.h"
#include "PaletteController.hpp"
#include "View.hpp"

class PaletteButton;
class SymbolistMainComponent;

/**
 * Describes the graphic component representing the palette.
 */
class PaletteComponent : public SymbolistComponent,
                         public View<SymbolistModel, PaletteController>
{
    Palette* palette_pointer;

public:
    PaletteComponent();
    ~PaletteComponent();
    
    void buildFromPalette(Palette* palette);
    void selectPaletteButton(int i);
    
    void paint (Graphics& g) override;
    
    /* Overrides the update method inherited from the Observer class. */
    inline void update() override { repaint(); }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PaletteComponent)

};

#endif
