#include "PaletteButton.hpp"
#include "PaletteComponent.h"

using namespace std ;

/********************
 * ONE BUTTON
 ********************/

PaletteButton::PaletteButton(int i, Symbol* s)
{
    button_id = i;
    
    /* Normally the PaletteButton being created is not
     * associated with a parent component yet.
     */
    PaletteComponent* paletteComponent = dynamic_cast<PaletteComponent* >(getParentComponent());
    if (paletteComponent != NULL)
        try
        {
            // May throw logic_error if PaletteController has no parent controller.
            graphic_comp = paletteComponent->getController()->makeComponentFromSymbol(s, false);
        }
        catch(logic_error& error)
        {
            cout << error.what() << endl;
            
            // Retrieves the SymbolistHandler instance to create component.
            graphic_comp = getSymbolistHandler()->makeComponentFromSymbol(s, false);
        }
    
    /* If no parent component then gets the SymbolistHandler instance directly. */
    else graphic_comp = getSymbolistHandler()->makeComponentFromSymbol(s, false);
    
    setComponentID("PaletteButton");
    addAndMakeVisible(graphic_comp);
}

void PaletteButton::setSelected(bool sel)
{
    selected = sel;
}

void PaletteButton::resized()
{
    graphic_comp->setBounds( getLocalBounds() );
    graphic_comp->resizeToFit( 5 , 5 , getWidth() - 10, getHeight() - 10  );
}

void PaletteButton::paint (Graphics& g)
{
    Colour button_color = selected ? Colours::black : Colour::fromFloatRGBA(0, 0, 0, 0.2);
    
    graphic_comp->setSymbolComponentColor( button_color );
    
    g.setColour( button_color );
    g.drawRect( getLocalBounds() );
    
}

void PaletteButton::mouseDown(const MouseEvent& event)
{
    PaletteComponent* parentComponent = dynamic_cast<PaletteComponent* >(getParentComponent());
    
    // Checks the downcast result.
    if (parentComponent != NULL) {
        parentComponent->selectPaletteButton(button_id);
    }
    
}
