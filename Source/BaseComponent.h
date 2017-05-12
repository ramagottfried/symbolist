
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

class BaseComponent : public Component
{
public:
    BaseComponent();
    ~BaseComponent();

    void mouseEnter( const MouseEvent& event ) override;
    void mouseMove( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseExit( const MouseEvent& event ) override;

    void moved () override;
    void resized () override;
    
    inline void attachScore(Component *s){ the_score = s; };
    inline Component *getScore(){ return the_score; };

    void paint ( Graphics& g );
    
    // add osc score w/r here?
    
protected:
    // parameters
    Rectangle<int>  bounds;
    float           strokeWeight = 1;
    Colour          color = Colours::black;
    
    // interaction
    Point<float>    m_down;
    bool            showBoundingBox = true;
    
private:
    Component *the_score;
    void *oscbundle;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BaseComponent)
    
};



// all parameters that might be used for performance should be stored in the score,
// separate from the graphic component class, if possible, generalize the stored parameter namespace here so that
// all inherited children can read and write their states through this interface