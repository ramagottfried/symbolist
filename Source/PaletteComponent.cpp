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
}

void PaletteButton::mouseDown ( const MouseEvent& event )
{
    cout << button_id << endl;
    static_cast<PaletteComponent*>( getParentComponent() )->selectPaletteButton(button_id);
}


/********************
 * PALETTE VIEW
 ********************/

PaletteComponent::PaletteComponent() {}

PaletteComponent::~PaletteComponent()
{
    deleteAllChildren();
}

void PaletteComponent::buildFromPalette(std::vector<std::shared_ptr<BaseComponent>> palette)
{
    for (int i = 0 ; i < palette.size(); i++ ) {
        
        PaletteButton *pb = new PaletteButton(i);
        pb->setTopLeftPosition(5, 5 + (i * 45));
        pb->setSize(40 , 40);
        addAndMakeVisible(pb);
    }
}

void PaletteComponent::selectPaletteButton(int i){
    for (int b = 0; b < getNumChildComponents(); b ++) {
        PaletteButton *button = static_cast<PaletteButton*>( getChildComponent(b) );
        if (b == i) button->setSelected(true);
        else button->setSelected(false);
    }
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
