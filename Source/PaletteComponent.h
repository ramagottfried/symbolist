#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "PrimitiveIncludes.h"

/*
 
 The palette component should make buttons from the
 the symbols, drawing a miniature version of the design.
 
 */


class PaletteComponent : public Component
{
    
public:
    PaletteComponent();
    ~PaletteComponent();
    
    void addSymbolButton(BaseComponent *c)
    {
        palette.add(c);
        addAndMakeVisible(c);
        addMouseListener(this, true);
    }
    
    void paint (Graphics& g) override
    {
    
    }
    
    void resized () override
    {}
    
    void mouseMove ( const MouseEvent& event ) override
    {}
    
    void mouseDown ( const MouseEvent& event ) override
    {}
    
    void mouseDrag ( const MouseEvent& event ) override
    {}
    
    
private:
    OwnedArray<BaseComponent> palette;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PaletteComponent)

};
