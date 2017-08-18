
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"
#include "BaseComponent.h"

//==============================================================================


class OSCOutputViewer : public Component
{
public:
    OSCOutputViewer(SymbolistHandler *sh ){}
    ~OSCOutputViewer(){}
    
    void paint (Graphics& g) override;
    void resized() override;
    
private:
    
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCOutputViewer)
};
