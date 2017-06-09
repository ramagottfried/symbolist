
#include "CircleComponent.h"

CircleComponent::CircleComponent(float x, float y,
                float w, float h,
                float stroke,
                Colour color ) :
    BaseComponent("circle" , x - (w * .5), y - (h * .5), w , h, stroke, color ),
    strokeType(stroke)
{
    auto area = getLocalBounds().toFloat().reduced( strokeWeight );;
    m_path.addEllipse (area.expanded (stroke * 0.5f));
    m_path.addEllipse (area.reduced  (stroke * 0.5f));
    m_path.setUsingNonZeroWinding (false);
}

void CircleComponent::symbol_paint ( Graphics& g )
{
    g.setColour( current_color );
    g.fillPath (m_path);
}

void CircleComponent::symbol_resized()
{
    auto b = getLocalBounds().toFloat().reduced( strokeWeight );;
    m_path.scaleToFit( b.getX(), b.getY(), b.getWidth(), b.getHeight(), false );
}