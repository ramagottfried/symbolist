#pragma once

#include "SymbolistComponent.h"

using namespace std;

class ScoreCursor : public SymbolistComponent
{
public:
    
    void paint( Graphics& g ) override
    {
        g.setColour( Colours::red );
        g.fillRect( getLocalBounds() );
    }
    
    void mouseDrag( const MouseEvent& event ) override
    {
        float x = event.getEventRelativeTo( getParentComponent() ).position.getX();
        m_playpoint = x * 0.01f;
        setBounds( x, 0, 2, getLocalBounds().getHeight() );
        
        auto sh = getSymbolistHandler();
        if( sh )
        {
            sh->symbolistAPI_setTime( m_playpoint );
            sh->symbolistAPI_getSymbolsAtTime( m_playpoint );
        }

    }
    
    void mouseDown( const MouseEvent& event ) override
    {
        m_down = event.position;
    }
    
    void resized() override
    {
        setBounds( m_playpoint * 100.0f, 0, 2, getLocalBounds().getHeight() );
    }
    
    inline void setPlayPoint( float t){ m_playpoint = t; }
    inline float getPlayPoint(){ return m_playpoint; }
    
private:
    float           m_playpoint = 1;
    
    Point<float>    m_down;
};