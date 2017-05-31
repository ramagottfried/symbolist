
#include "PaletteComponent.h"

using namespace std ;


/********************
 * ONE BUTTON
 ********************/

void PaletteButton::setSelected(bool sel)
{
    selected = sel;
}

void PaletteButton::paint (Graphics& g)
{
    if (selected) g.fillAll( Colours::grey );
    else g.fillAll( Colours::lightgrey );
    
    auto symbol_template = static_cast<PaletteComponent*>( getParentComponent() )->getPaletteItem(button_id);
    g.setOrigin(symbol_template->getX(), symbol_template->getY());
    symbol_template->symbol_paint(g);
    //cout << symbol_template->getSymbolType() << endl ;
}

void PaletteButton::mouseDown ( const MouseEvent& event )
{
    PaletteComponent* pv = static_cast<PaletteComponent*>( getParentComponent() );
    pv->selectPaletteButton(button_id);
}


/********************
 * PALETTE VIEW
 ********************/

PaletteComponent::PaletteComponent() {}

PaletteComponent::~PaletteComponent()
{
    deleteAllChildren();
}

void PaletteComponent::buildFromPalette(SymbolistPalette* palette)
{
    palette_pointer = palette;
    
    for (int i = 0 ; i < palette->getPaletteNumItems() ; i++ )
    {
        PaletteButton *pb = new PaletteButton(i);
        pb->setTopLeftPosition(5, 5 + (i * 45));
        pb->setSize(40 , 40);
        addAndMakeVisible(pb);
    }
}

BaseComponent* PaletteComponent::getPaletteItem(int i)
{
    return palette_pointer->getPaletteItem(i);
}

void PaletteComponent::selectPaletteButton(int i)
{
    for (int b = 0; b < getNumChildComponents(); b ++)
    {
        PaletteButton *button = static_cast<PaletteButton*>( getChildComponent(b) );
        if (b == i) button->setSelected(true);
        else button->setSelected(false);
    }
    palette_pointer->setSelectedItem(i);
    repaint();
}


void PaletteComponent::paint (Graphics& g)
{
    /*
    // all this is just to draw a vertical line ?? ;-p
    g.setColour( Colours::black );
    Path p, _p;
    PathStrokeType strokeType(0.5);
    float dashes[] = {1.0, 2.0};
    p.startNewSubPath( getWidth(), 0 );
    p.lineTo(getWidth(), getHeight() );
    strokeType.createDashedStroke(p, p, dashes, 2 );
    g.strokePath(p, strokeType );
    */
}
