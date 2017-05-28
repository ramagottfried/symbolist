
#include "CircleComponent.h"

CircleComponent::CircleComponent()
{
    setComponentID ( "Circle" );
    // should this constructor exist? => probably not, or to set default values
}

CircleComponent::CircleComponent( float center_x, float center_y, float w, float h, float stroke, Colour color )
{
    setComponentID ( "Circle" );
    setBounds( center_x - (w * .5), center_y - (h * .5), w , h);
    strokeWeight = stroke;
    sym_color = color;
    symbol_type = String("circle");
}


void CircleComponent::symbol_paint ( Graphics& g )
{
    g.setColour( current_color );
    const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( strokeWeight );
//    printf("paint %p %f %f %f %f\n", this, bounds.getX(), bounds.getY(), bounds.getWidth(), bounds.getHeight() );
    g.drawEllipse ( bounds, (float) strokeWeight );
    
}
