
#include "CircleComponent.h"

CircleComponent::CircleComponent()
{
    setComponentID ( "Circle" );
    // should this constructor exist?
}

CircleComponent::CircleComponent( float x, float y, float radius, float stroke, Colour color )
{
    setComponentID ( "Circle" );
    setBounds ( x-radius, y-radius, radius+radius, radius+radius );
    strokeWeight = stroke;
    sym_color = color;
    
}


void CircleComponent::symbol_paint ( Graphics& g )
{
    g.setColour( current_color );
    const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( strokeWeight );
    g.drawEllipse ( bounds, (float) strokeWeight );
    
}
