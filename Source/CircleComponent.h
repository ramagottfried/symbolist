#pragma once

#include "BaseComponent.h"

class CircleComponent : public BaseComponent
{
public:
    CircleComponent();
    CircleComponent( float x, float y, float diameter = 10 );
    ~CircleComponent();
    
    void paint ( Graphics& g ) override;
    void moved () override;
    
    virtual void mouseDoubleClick (const MouseEvent& event) override;
    
    void mouseEnter( const MouseEvent& event ) override;
    void mouseMove( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseExit( const MouseEvent& event ) override;
    
    // getters
    
    inline int getStrokeWeightFromScore() { return m_strokeWeight; }
    inline float getDiameter() { return m_diameter; }
    inline Point<float> getPositionFromScore()
    {
        // the shape has a position set by the parent via setBounds, we can access it here also via getPosition
        // however, this should querry the score actually
        
        return m_pos;
    }
    
private:
    Point<float> m_pos;
    int m_strokeWeight = 2;
    float m_diameter = 10;
    Colour m_color = Colours::black;
    
    Point<float> m_down;
    Rectangle<int> m_bounds;

    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CircleComponent)
};
