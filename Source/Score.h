#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "Shapes.h"


class ScoreComponent : public Component
{
public:
    ScoreComponent()
    {
    }
    
    ~ScoreComponent()
    {
        for ( int i = 0; i < score_stack.size(); i++ )
        {
           delete score_stack[i];
        }
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
        /*
        std::cout << event.eventComponent->getComponentID() << "\n" ;

        MouseEvent scoreEvent = event.getEventRelativeTo(this);
        
        printf ( "score move at %f %f\n", scoreEvent.position.getX(), scoreEvent.position.getY() );
         */
    }
    
    void mouseDown ( const MouseEvent& event ) override
    {
        
        if ( event.mods.isShiftDown() )
        {
            CircleComponent *circle = new CircleComponent( event.position.getX(), event.position.getY(), 100 );
            addAndMakeVisible ( circle );
            circle->addMouseListener(this, false);
            score_stack.emplace_back ( circle );
        }
        else
            m_down = event.position;
        
    }

    void mouseDrag ( const MouseEvent& event ) override
    {
        
        if( event.eventComponent->getComponentID() == "Circle" )
        {
            MouseEvent scoreEvent = event.getEventRelativeTo(this);
            
            Rectangle<int> bounds = event.eventComponent->getBounds();
            
            event.eventComponent->setBounds (   scoreEvent.getPosition().getX() - m_down.getX(),
                                                scoreEvent.getPosition().getY() - m_down.getY(),
                                                bounds.getWidth(),
                                                bounds.getHeight() );
            
        }

    }

private:
    Point<float> m_down;
    std::vector<Component *> score_stack;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreComponent)
};
