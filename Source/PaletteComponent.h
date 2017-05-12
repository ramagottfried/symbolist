#pragma once

#include "../JuceLibraryCode/JuceHeader.h"


class PaletteComponent : public Component
{
    
public:
    PaletteComponent();
    ~PaletteComponent();
    
private:
    OwnedArray<Component> palette;
    
    template <typename ComponentType>
    ComponentType   *palette_add(ComponentType *newComp);
  
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PaletteComponent)

};