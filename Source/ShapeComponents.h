#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

class ScoreComponenet : public Component
{
public:
    ScoreComponenet(){}
    ~ScoreComponenet(){}
    
    void paint (Graphics& g) override
    {}
    
    void resized() override
    {}
    
private:
    void *osc;
    int score_id = 0;
  
};


class CircleComponent : public Component
{
public:
    CircleComponent(){}
    ~CircleComponent(){}
    
    void set( float x, float y, float r)
    {
        m_x = x; m_y = y; m_radius = r;
        float half_r = r * 0.5;
        setBounds (x-m_radius*half_r, y-m_radius*half_r, x+half_r, y+half_r);

        printf("circle %s %f %f\n", __func__, x, y);
    }
    
    void paint ( Graphics& g ) override
    {
        g.setColour (Colours::white);
        g.drawEllipse ( m_x, m_y, m_radius, m_radius, 1 );
    }
    
private:
    float m_radius = 10;
    float m_x = 100;
    float m_y = 100;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CircleComponent)
};

class LineComponent : public Component
{
public:
    LineComponent(){}
    ~LineComponent(){}
    
    inline void set( float x1, float y1, float x2, float y2)
    {
        m_x1 = x1; m_y1 = y1; m_x2 = x2; m_y2 = y2;
//        repaint();
    }
    
    void paint ( Graphics& g ) override
    {
        printf("line %s\n", __func__);
        g.drawLine( m_x1, m_y1, m_x2, m_y2);
    }
    
private:
    float m_x1, m_x2, m_y1, m_y2;

    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (LineComponent)

    
};