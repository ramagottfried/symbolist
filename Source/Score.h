#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "Shapes.h"


class ScoreComponent : public Component
{
public:
    ScoreComponent()
    {
        addMouseListener(this, true);
        addAndMakeVisible (circle);
        addAndMakeVisible (line);
    }
    
    ~ScoreComponent()
    {
    }
    
    void paint (Graphics& g) override
    {
        g.fillAll ( Colours::white );
    }
    
    void resized () override
    {
    }
    
    void mouseMove ( const MouseEvent& event ) override
    {
        std::cout << event.eventComponent->getComponentID() << "\n" ;

        MouseEvent scoreEvent = event.getEventRelativeTo(this);
        
        printf ( "score move at %f %f\n", scoreEvent.position.getX(), scoreEvent.position.getY() );
    }
    
    void mouseDown ( const MouseEvent& event ) override
    {
        m_down = event.position;
        
        if ( event.mods.isShiftDown() )
        {
            circle.setBounds ( m_down.getX(), m_down.getY(), 100, 100 );
        }
    }

    void mouseDrag ( const MouseEvent& event ) override
    {
        
        if( event.eventComponent->getComponentID() == "Circle" )
        {
            MouseEvent scoreEvent = event.getEventRelativeTo(this);
            Rectangle<int> bounds = event.eventComponent->getBounds();
            event.eventComponent->setBounds ( scoreEvent.getPosition().getX(), scoreEvent.getPosition().getY(), bounds.getWidth(), bounds.getHeight() );
            
        }

    }

private:
    Point<float> m_down;
    CircleComponent circle;
    LineComponent line;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreComponent)
};
