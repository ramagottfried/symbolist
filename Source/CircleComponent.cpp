
#include "CircleComponent.h"

CircleComponent::CircleComponent()
{
    setComponentID ( "Circle" );
    // should this constructor exist?
}

CircleComponent::CircleComponent( Point<float> startPT ) : BaseComponent(startPT)
{
    setComponentID ( "Circle" );    
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
//    printf("paint %p %f %f %f %f\n", this, bounds.getX(), bounds.getY(), bounds.getWidth(), bounds.getHeight() );
    g.drawEllipse ( bounds, (float) strokeWeight );
    
}
