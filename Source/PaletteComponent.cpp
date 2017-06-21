
#include "PaletteComponent.h"
#include "SymbolistMainComponent.h"


using namespace std ;


/********************
 * ONE BUTTON
 ********************/

PaletteButton::PaletteButton( int i, Symbol *s)
{
    button_id = i;
    graphic_comp = SymbolistHandler::makeComponentFromSymbol(s);
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
    graphic_comp->setBounds( 0 , 0 , getWidth() , getHeight() );
    graphic_comp->resizeToFit( 0 , 0 , getWidth() , getHeight() );
}

void PaletteButton::paint (Graphics& g)
{
    if (selected) g.fillAll( Colours::lightgrey );
    else g.fillAll( Colours::white );
    //g.setOrigin(getX()+((getWidth()-graphic_comp->getWidth())/2),
    //            getY()+((getHeight()-graphic_comp->getHeight())/2));
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

    Symbol s("path", 0, 0, 30, 30);
    
    OSCMessage numSeg_mess( "/numSegments",         (int32)3 );
    OSCMessage type_mess0(  "/segment/0/type",      (String)"cubic" );
    OSCMessage x_mess0(     "/segment/0/x_points",  4.0f, 14.0f, 22.0f, 16.0f );
    OSCMessage y_mess0(     "/segment/0/y_points",  4.0f, 2.0f, 8.0f, 14.0f );
    OSCMessage type_mess1(  "/segment/1/type",      (String)"cubic" );
    OSCMessage x_mess1(     "/segment/1/x_points",  16.0f, 12.0f, 14.0f, 20.0f );
    OSCMessage y_mess1(     "/segment/1/y_points",  14.0f, 20.0f, 24.0f, 22.0f );
    OSCMessage type_mess2(  "/segment/1/type",      (String)"cubic" );
    OSCMessage x_mess2(     "/segment/1/x_points",  20.0f, 24.0f, 28.0f, 36.0f );
    OSCMessage y_mess2(     "/segment/1/y_points",  22.0f, 24.0f, 28.0f, 36.0f );
    
    s.addOSCMessage(numSeg_mess);
    s.addOSCMessage(type_mess0);
    s.addOSCMessage(x_mess0);
    s.addOSCMessage(y_mess0);
    s.addOSCMessage(type_mess1);
    s.addOSCMessage(x_mess1);
    s.addOSCMessage(y_mess1);
    s.addOSCMessage(type_mess2);
    s.addOSCMessage(x_mess2);
    s.addOSCMessage(y_mess2);
    
    PaletteButton *pb = new PaletteButton(-1, &s);
    pb->setTopLeftPosition(10, 20);
    pb->setSize(30 , 30);
    addAndMakeVisible(pb);

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
        pb->setTopLeftPosition(14, 80 + (i * 30));
        pb->setSize(22 , 22);
        addAndMakeVisible(pb);
    }
}


void PaletteComponent::selectPaletteButton(int i)
{
    for (int b = 0; b < getNumChildComponents(); b ++)
    {
        PaletteButton *button = (PaletteButton*) getChildComponent(b);
        if ( button->getID() == i ) button->setSelected(true);
        else button->setSelected(false);
    }
    if ( i >= 0) palette_pointer->setSelectedItem(i);
    
    SymbolistMainComponent* smc = (SymbolistMainComponent*)getParentComponent();
    if ( smc != NULL )
    {
        if ( i >= 0) {
            smc->setDrawMode(UI_DrawType::from_template);
        }
        else
        { // specialModes
            smc->setDrawMode(UI_DrawType::free_draw);
        }
    }
    repaint();
}


void PaletteComponent::paint (Graphics& g)
{
    g.fillAll( Colours::white );
    g.setColour(Colours::lightgrey);
    g.drawLine(getWidth(),0,getWidth(),getHeight());
}


