#include "PaletteComponent.h"



PaletteComponent::PaletteComponent()
{
    setComponentID("PaletteComponent");
    // if this will be user definable this should be a reference to the symbol
    // buttons are really just regular components, so we can just ignore the button class (as suggested in the JUCE h files)
    
    addSymbolButton(new CircleComponent( 0, 0 ) );
    
}


PaletteComponent::~PaletteComponent(){}


void PaletteComponent::addSymbolButton(BaseComponent *c)
{
    m_palette_symbol.add(c);
    addAndMakeVisible(c);
    addMouseListener(this, true);
}

void PaletteComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );
    g.setColour( Colours::black );
    
    Path p, _p;
    PathStrokeType strokeType(0.5);
    float dashes[] = {1.0, 2.0};
    
    p.startNewSubPath( getWidth(), 0 );
    p.lineTo(getWidth(), getHeight() );
    
    strokeType.createDashedStroke(p, p, dashes, 2 );
    g.strokePath(p, strokeType );
    
}

void PaletteComponent::resized ()
{
    Rectangle<int> pallete_bounds = getBoundsInParent();
//    printf("%i %i %i %i\n", pallete_bounds.getX(), pallete_bounds.getY(), pallete_bounds.getWidth(), pallete_bounds.getHeight() );

    for(BaseComponent* c : m_palette_symbol )
    {
        c->setBounds( pallete_bounds.getX(), pallete_bounds.getY(), pallete_bounds.getWidth(), 20) ;
    }

}