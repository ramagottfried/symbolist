
#include "PaletteComponent.h"
#include "MainComponent.h"


using namespace std ;


/********************
 * ONE BUTTON
 ********************/

PaletteButton::PaletteButton( int i, Symbol *s)
{
    button_id = i;
    graphic_comp = SymbolistMainComponent::makeComponentFromSymbol(s);
    setComponentID("PaletteButton");
}

PaletteButton::~PaletteButton()
{
    delete graphic_comp;
}


void PaletteButton::setSelected(bool sel)
{
    selected = sel;
}

void PaletteButton::resized()
{
    graphic_comp->setBounds( 8 , 8 , getWidth()-16 , getHeight()-16 );
}

void PaletteButton::paint (Graphics& g)
{
    if (selected) g.fillAll( Colours::grey );
    else g.fillAll( Colours::lightgrey );
    g.setOrigin(graphic_comp->getX(), graphic_comp->getY());
    graphic_comp->paint(g);
}

void PaletteButton::mouseDown ( const MouseEvent& event )
{
    PaletteComponent* pv = (PaletteComponent*) getParentComponent();
    pv->selectPaletteButton(button_id);
}


/********************
 * PALETTE VIEW
 ********************/

PaletteComponent::PaletteComponent()
{
    setComponentID("PaletteComponent");
    std::cout << "PaletteComponent " << this << std::endl;

}

PaletteComponent::~PaletteComponent()
{
    deleteAllChildren();
}

void PaletteComponent::buildFromPalette(SymbolistPalette* palette)
{
    palette_pointer = palette;
    
    for (int i = 0 ; i < palette->getPaletteNumItems() ; i++ )
    {
        PaletteButton *pb = new PaletteButton(i, palette->getPaletteItem(i));
        pb->setTopLeftPosition(5, 5 + (i * 45));
        pb->setSize(40 , 40);
        addAndMakeVisible(pb);
    }
}


void PaletteComponent::selectPaletteButton(int i)
{
    for (int b = 0; b < getNumChildComponents(); b ++)
    {
        PaletteButton *button = (PaletteButton*) getChildComponent(b);
        if (b == i) button->setSelected(true);
        else button->setSelected(false);
    }
    palette_pointer->setSelectedItem(i);
    repaint();
}

