
#include "MouseModeComponent.hpp"
#include "SymbolistMainComponent.h"


void MouseModeComponent::paint( Graphics &g )
{
    
    g.setFont (Font (16.0f));
    g.setColour (Colours::grey);
    g.drawText (m_str, getLocalBounds() , Justification::topLeft, false);
    
}

void MouseModeComponent::drawString()
{
    if ( m_editType == UI_EditType::SELECTION )
    {
        m_str = " select " ;
    }
    else if ( m_drawType == UI_DrawType::FROM_TEMPLATE )
    {
        m_str = " draw " ;
        m_str += getSymbolistHandler()->getCurrentSymbol()->getType();
    }
    else
    {
        m_str = " draw lines " ;
    }
    
    repaint();
}

void MouseModeComponent::setMouseMode( UI_EditType t )
{
    m_editType = t;
    drawString();
}

void MouseModeComponent::setDrawMode( UI_DrawType t )
{
    m_drawType = t;
    drawString();
}
