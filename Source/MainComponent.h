
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "ScoreComponent.h"
#include "ScoreData.h"
#include "PaletteComponent.h"


class MainComponent   : public Component
{
public:
    //==============================================================================
    MainComponent();
    MainComponent( Score *s );
    
    ~MainComponent();

    void paint (Graphics&) override;
    void resized() override;

    void buttonCallback(MouseEvent *event, int type)
    {
        std::cout << event->eventComponent->getName() << type << std::endl;
    }
    
private:
    
    ScoreComponent scoreGUI;
    
    //DrawableButton *dbutton = NULL;
//    PaletteComponent palette{this};
    
    OwnedArray<Component> palette; // << this should be dynamically expandable 

    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};
