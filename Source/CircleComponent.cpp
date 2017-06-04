
#include "CircleComponent.h"


void CircleComponent::symbol_paint ( Graphics& g )
{
    g.setColour( current_color );
    const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( strokeWeight );
    //printf("paint %p %f %f %f %f\n", this, bounds.getX(), bounds.getY(), bounds.getWidth(), bounds.getHeight() );
    g.drawEllipse ( bounds, (float) strokeWeight );
    
}
