#include "ScoreCursor.h"

void ScoreCursor::paint( Graphics& g )
{
    if( display && m_playpoint >= 0 )
    {
        g.setColour( Colours::lightblue );
        auto b = getLocalBounds();
        g.drawLine( b.getCentreX(), 0, b.getCentreX(), b.getBottom() );
    }
}

void ScoreCursor::mouseDrag( const MouseEvent& event )
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
        auto sym = staff->getScoreSymbol();
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

void ScoreCursor::mouseDown( const MouseEvent& event )
{
    m_down = event.position;
}

void ScoreCursor::resized()
{
    // setPlayPoint( m_playpoint );
}

void ScoreCursor::toggleDisplayState()
{
    display = !display;
    if( display )
        setPlayPoint(m_playpoint);
    else
        repaint();
}

void ScoreCursor::setPlayPoint( float t )
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
        
        Symbol* sym = staff->getScoreSymbol();
        
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

