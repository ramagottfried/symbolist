
#include "CircleComponent.h"

CircleComponent::CircleComponent(const Symbol &s) : BaseComponent( s )
{
<<<<<<< Updated upstream
    
    auto area = getLocalBounds().toFloat().reduced( strokeWeight );;
    auto hw = area.getWidth() * 0.5f;
    auto hw55 = hw * 0.55f;
    auto hh = area.getHeight() * 0.5f;
    auto hh55 = hh * 0.55f;
    auto cx = area.getX() + hw;
    auto cy = area.getY() + hh;
    
    m_path.startNewSubPath (cx, cy - hh);
    m_path.cubicTo (cx + hw55, cy - hh, cx + hw, cy - hh55, cx + hw, cy);
    m_path.cubicTo (cx + hw, cy + hh55, cx + hw55, cy + hh, cx, cy + hh);
    m_path.cubicTo (cx - hw55, cy + hh, cx - hw, cy + hh55, cx - hw, cy);
    m_path.cubicTo (cx - hw, cy - hh55, cx - hw55, cy - hh, cx, cy - hh);
    m_path.closeSubPath();
=======
    symbol_type = "circle";
    auto area = getLocalBounds().toFloat().reduced( strokeWeight );;
    m_path.addEllipse (area.expanded (2 * 0.5f));
    m_path.addEllipse (area.reduced  (2 * 0.5f));
    m_path.setUsingNonZeroWinding (false);
>>>>>>> Stashed changes
}

void CircleComponent::paint ( Graphics& g )
{
    g.setColour( current_color );
    g.strokePath(m_path, strokeType);
}

void CircleComponent::resized()
{
    BaseComponent::resized();
    auto b = getLocalBounds().toFloat().reduced( strokeWeight );;
    m_path.scaleToFit( b.getX(), b.getY(), b.getWidth(), b.getHeight(), false );
}

<<<<<<< Updated upstream
=======

void CircleComponent::importFromSymbol()
{
    BaseComponent::importFromSymbol();
    setTopLeftPosition( getX() - ( getWidth() * 0.5 ), getY() - ( getHeight() * 0.5 ) ) ;
}
>>>>>>> Stashed changes
