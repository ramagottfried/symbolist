#pragma once

#include "../JuceLibraryCode/JuceHeader.h"


class BaseComponenet : public Component
{
public:
    BaseComponenet(){}
    ~BaseComponenet(){}
    
private:
    void *oscbundle;
    
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

    CircleComponent( float x, float y, float diameter = 10 )
    {
        setComponentID ( "Circle" );
        setBounds ( x, y, diameter, diameter );
        m_diameter = diameter;

    }
    
    ~CircleComponent(){}
    
    
    void paint ( Graphics& g ) override
    {
        g.setColour( m_color );
        
        const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( m_strokeWeight );
        
        g.drawEllipse ( bounds, (float) m_strokeWeight );
    }
    
    void moved () override
    {
        m_pos = getPosition().toFloat();
        // push to score here?
        
//        std::cout << m_pos.getX() << " " << m_pos.getY() << "\n";
    }
    
    virtual void mouseDoubleClick (const MouseEvent& event) override
    {
        printf("2x click");
    }
    
    void mouseEnter( const MouseEvent& event ) override
    {
        m_color = Colours::hotpink;
        repaint();
    }
    
    void mouseMove( const MouseEvent& event ) override
    {
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
    
    int getStrokeWeightFromScore()
    {
        return m_strokeWeight;
    }

    
    Point<float> getPositionFromScore()
    {
        // the shape has a position set by the parent via setBounds, we can access it here also via getPosition
        // however, this should querry the score actually
        
        return m_pos;
    }
    
    float getDiameter()
    {
        return m_diameter;
    }
    
    
private:
    Point<float> m_pos;
    
    int m_strokeWeight = 2;
    float m_diameter = 10;
    Colour m_color = Colours::black;
    
    
    // all parameters that might be used for performance should be stored in the score,
    // separate from the graphic component class
    
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