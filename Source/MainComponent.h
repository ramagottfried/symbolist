
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "ScoreComponent.h"
#include "ScoreData.h"
#include "PaletteComponent.h"


class MainComponent   : public Component//, public Button::Listener
{
public:
    //==============================================================================
    MainComponent();
    MainComponent( Score *s );
    
    ~MainComponent();

    void paint (Graphics&) override;
    void resized() override;

//    void buttonClicked (Button* button) override;
    
private:
    
    ScoreComponent scoreGUI;
    
//    PaletteComponent palette;
        
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};
