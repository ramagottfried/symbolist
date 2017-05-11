#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "PrimitiveIncludes.h"

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
    
    void addChildComponentOSC( BaseComponent *c );
    
    inline Point<float> getScoreMouseDown(){ return m_down; }
    
private:
    Point<float> m_down;
    std::vector<BaseComponent *> score_stack;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreComponent)
};
