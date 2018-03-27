#include "PaletteButton.hpp"
#include "PaletteComponent.h"

using namespace std ;

/********************
 * ONE BUTTON
 ********************/

PaletteButton::PaletteButton(int i, shared_ptr<Symbol> s)
{
    button_id = i;
    graphic_comp = getSymbolistHandler()->makeComponentFromSymbol(s, false);
    setComponentID("PaletteButton");
    addAndMakeVisible(graphic_comp);
}

PaletteButton::~PaletteButton()
{
    //    cout << "deleting button " << this << endl;
}

void PaletteButton::setSelected(bool sel)
{
    selected = sel;
}

void PaletteButton::resized()
{
    graphic_comp->setBounds( getLocalBounds() );
    graphic_comp->resizeToFit( 5 , 5 , getWidth() -10, getHeight() -10  );
}

void PaletteButton::paint (Graphics& g)
{
    Colour button_color = selected ? Colours::black : Colour::fromFloatRGBA(0, 0, 0, 0.2);
    
    graphic_comp->setSymbolComponentColor( button_color );
    
    g.setColour( button_color );
    g.drawRect( getLocalBounds() );
    
}

void PaletteButton::mouseDown ( const MouseEvent& event )
{
    PaletteComponent* pv = dynamic_cast<PaletteComponent*>(getParentComponent());
    
    // Checks the downcast result.
    if (pv != NULL) {
        pv->selectPaletteButton(button_id);
    }
    
}
