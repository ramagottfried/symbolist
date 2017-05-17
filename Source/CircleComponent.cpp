
#include "CircleComponent.h"

CircleComponent::CircleComponent()
{
    setComponentID ( "Circle" );
}

// add options for other params: color, stroke...
CircleComponent::CircleComponent( float x, float y, float diameter, float stroke, Colour color )
{

    setComponentID ( "Circle" );
    setBounds ( x, y, diameter, diameter );
    strokeWeight = stroke;
    sym_color = color;
    
}



CircleComponent::~CircleComponent(){}


void CircleComponent::symbol_paint ( Graphics& g )
{
    g.setColour( current_color );
    const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( strokeWeight );
    g.drawEllipse ( bounds, (float) strokeWeight );
    
}


void CircleComponent::symbol_mouseDoubleClick (const MouseEvent& event)
{
    printf("2x click");
}

/*
 
void CircleComponent::mouseEnter( const MouseEvent& event )
{
}

void CircleComponent::mouseMove( const MouseEvent& event )
{
}

void CircleComponent::mouseDown( const MouseEvent& event )
{
       // at the moment mouse down is only for creating new objects
}

void CircleComponent::mouseDrag( const MouseEvent& event )
{
    
}

void CircleComponent::mouseExit( const MouseEvent& event )
{
    BaseComponent::mouseExit( event );
    
    current_color = Colours::black;
    repaint();
}
*/