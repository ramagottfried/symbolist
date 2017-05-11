#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "Shapes.h"


class ScoreComponent : public Component
{
public:
    ScoreComponent();
    ~ScoreComponent();
    
    void paint (Graphics& g) override;
    void resized () override;
    
    void mouseMove ( const MouseEvent& event ) override;
    void mouseDown ( const MouseEvent& event ) override;
    void mouseDrag ( const MouseEvent& event ) override;
    
    void addChildComponentOSC( Component *c );
    
private:
    Point<float> m_down;
    std::vector<Component *> score_stack;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreComponent)
};
