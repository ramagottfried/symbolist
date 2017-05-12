
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "ScoreComponent.h"
#include "ScoreData.h"


class MainComponent   : public Component
{
public:
    //==============================================================================
    MainComponent();
    MainComponent( Score& s );
    
    ~MainComponent();

    void paint (Graphics&) override;
    void resized() override;

private:
    
    ScoreComponent scoreGUI;
        
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};
