#pragma once

#include "SymbolistComponent.h"
#include "StaffComponent.hpp"

using namespace std;

class ScoreCursor : public SymbolistComponent
{
public:
    
    ScoreCursor(){}
    
    ~ScoreCursor(){}
    
    void paint( Graphics& g ) override
    {
        if( display && m_playpoint >= 0 )
        {
            g.setColour( Colours::lightblue );
            auto b = getLocalBounds();
            g.drawLine( b.getCentreX(), 0, b.getCentreX(), b.getBottom() );
        }
    }
    
    void mouseDrag( const MouseEvent& event ) override
    {
        float x = event.getEventRelativeTo( getParentComponent() ).position.getX();
        
        m_playpoint = x * 0.01f;
        
        auto sh = getSymbolistHandler();
        if( sh )
        {
            sh->symbolistAPI_setTime( m_playpoint );
            sh->symbolistAPI_getSymbolsAtTime( m_playpoint );
        }
        
        setBounds( x, 0, 2, getLocalBounds().getHeight() );

    }
    
    void mouseDown( const MouseEvent& event ) override
    {
        m_down = event.position;
    }
    
    void resized() override
    {
        // setPlayPoint( m_playpoint );
    }
    
    void toggleDisplayState()
    {
        display = !display;
        if( display )
            setPlayPoint(m_playpoint);
    }
    
    void setPlayPoint( float t )
    {
        m_playpoint = t;
        
        auto staff = getSymbolistHandler()->getStaveAtTime(t);
        if( !staff )
        {
            setVisible(false);
        }
        else if( display )
        {
            setVisible(true);

            Symbol* sym = staff->getScoreSymbolPointer();
            
            float play_t = t - sym->getTime();
            auto staff_b = staff->getBoundsInParent();
            float play_x = staff_b.getX() + sym->timeToPixels( play_t );
            
            setBounds( play_x, staff_b.getY()-5, 5, staff_b.getHeight()+5 );
        }
        repaint();
    }
    
    inline float getPlayPoint(){ return m_playpoint; }
    
private:
    float           m_playpoint = -1;
    bool            display = false;
    
    Point<float>    m_down;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreCursor)
};
