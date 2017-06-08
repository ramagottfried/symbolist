
#include "CircleComponent.h"


void CircleComponent::symbol_paint ( Graphics& g )
{
    g.setColour( current_color );
    const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( strokeWeight );
    g.drawEllipse ( bounds, (float) strokeWeight );
}
