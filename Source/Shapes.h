#pragma once

#include "../JuceLibraryCode/JuceHeader.h"


class BaseComponenet : public Component
{
public:
    BaseComponenet(){}
    ~BaseComponenet(){}
    
private:
    void *osc;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BaseComponenet)
    
};


class CircleComponent : public Component
{
public:
    CircleComponent()
    {
        setComponentID ( "Circle" );
    }

    ~CircleComponent(){}
    
    void paint ( Graphics& g ) override
    {
        g.setColour( m_color );
        
        const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( m_strokeWeight );
        
        g.drawEllipse ( bounds, (float) m_strokeWeight );
    }
    
    virtual void mouseDoubleClick (const MouseEvent& event) override
    {
        printf("2x click");
    }
    
    
    void mouseEnter( const MouseEvent& event ) override
    {
        m_color = Colours::darkmagenta;
        repaint();
    }
    
    void mouseMove( const MouseEvent& event ) override
    {
     //   printf ( "circle %s at %f %f\n", __func__, event.position.getX(), event.position.getY() );
    }
    
    void mouseDown( const MouseEvent& event ) override
    {
    }
    
    void mouseDrag( const MouseEvent& event ) override
    {
    }
    
    void mouseExit( const MouseEvent& event ) override
    {
        m_color = Colours::black;
        repaint();
    }
    
private:
    int m_strokeWeight = 2;
    float m_radius = 10;
    Colour m_color = Colours::black;
    
    std::function<void ( const MouseEvent& event )> m_callback;

    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CircleComponent)
};



class LineComponent : public Component
{
public:
    LineComponent(){}
    ~LineComponent(){}
    
    inline void set( float x1, float y1, float x2, float y2){ m_x1 = x1; m_y1 = y1; m_x2 = x2; m_y2 = y2; };
    
    void paint ( Graphics& g ) override
    {
        g.drawLine( m_x1, m_y1, m_x2, m_y2);
    }
    
private:
    float m_x1, m_x2, m_y1, m_y2;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (LineComponent)
    
    
};