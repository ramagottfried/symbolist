#pragma once

#include "BaseComponent.h"

class CircleComponent : public BaseComponent
{
public:
    CircleComponent();
    CircleComponent( float x, float y, float diameter = 10, float stroke = 2 );
    ~CircleComponent();
    
    void paint ( Graphics& g );
    void moved () override;
    
    virtual void mouseDoubleClick (const MouseEvent& event) override;
    
    void mouseEnter( const MouseEvent& event ) override;
    void mouseMove( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseExit( const MouseEvent& event ) override;
    
    // getters
    
//    inline int getStrokeWeightFromScore() { return m_strokeWeight; }
//    inline float getDiameter() { return m_diameter; }
    
private:
    // local parameters for this shape
    float   m_diameter = 10;
    Colour  m_color = Colours::black;

    /*
     inherited:
        
     bounds (x,y,w,h)
     sel_color
     strokeweight
     
     */
    
    
    Point<float>    m_down;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CircleComponent)
};
