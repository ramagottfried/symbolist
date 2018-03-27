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
        // todo: keep the mouse drag on the right staff (basically where it was before)
        // then have to recalculate the playpoint accordingly
        
        auto sh = getSymbolistHandler();
        
        if( sh )
        {
            auto delta = event.getPosition() - m_prev_dragpoint;
            m_playpoint += (delta.x * 0.01f);
            m_prev_dragpoint = event.getPosition();
            
            auto staff = sh->getStaveAtTime( m_playpoint );
            
            if( !staff )
                return;
            
            sh->symbolistAPI_setTime( m_playpoint );
            
            // add output callback
            // sh->symbolistAPI_getSymbolsAtTime( m_playpoint );
            auto sym = staff->getScoreSymbolPointer();
            float play_t = m_playpoint - sym->getTime();
            auto staff_b = staff->getBoundsInParent();
            float play_x = staff_b.getX() + sym->timeToPixels( play_t );
            
            float y = staff_b.getY()-5;
            float h = staff_b.getHeight()+5;
            h = (h > minimum_size ? h : minimum_size);
            setBounds( play_x, y, 5, h );
            
//            float x = staff_c->getX() + event.position.getX();

  //          setBounds( x, 0, 2, getLocalBounds().getHeight() );
        }

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
        else
            repaint();
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

            shared_ptr<Symbol> sym = staff->getScoreSymbolPointer();
            
            float play_t = t - sym->getTime();
            auto staff_b = staff->getBoundsInParent();
            float play_x = staff_b.getX() + sym->timeToPixels( play_t );
            
            float y = staff_b.getY()-5;
            float h = staff_b.getHeight()+5;
            h = (h > minimum_size ? h : minimum_size);
            setBounds( play_x, y, 5, h );
        }
        repaint();
    }
    
    inline float getPlayPoint(){ return m_playpoint; }
    
private:
    float           m_playpoint = -1;
    bool            display = false;
    
    Point<float>    m_down;
    
    float           minimum_size = 50;
    
    Point<int>    m_prev_dragpoint;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreCursor)
};
