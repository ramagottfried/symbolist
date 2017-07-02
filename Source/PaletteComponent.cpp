
#include "PaletteComponent.h"
#include "SymbolistMainComponent.h"


using namespace std ;


/********************
 * ONE BUTTON
 ********************/

PaletteButton::PaletteButton( int i, Symbol *s)
{
    button_id = i;
    graphic_comp = getSymbolistHandler()->makeComponentFromSymbol(s,false);
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
}

PaletteComponent::~PaletteComponent()
{
    cout << "deleting all children of palette " << this << endl;
    deleteAllChildren();
}


void PaletteComponent::buildFromPalette(SymbolistPalette* palette)
{
    palette_pointer = palette;
    
    deleteAllChildren();
    
    // default "draw" button
    Symbol s("path", 0, 0, 30, 30);
    s.addOSCMessage( OSCMessage("/num_sub_paths", 1) );
    s.addOSCMessage( OSCMessage("/path/0/str", String("m 4. 4. c 14. 2. 22. 8. 16. 14. c 12. 20. 14. 24. 20. 22.")) );
    PaletteButton *pb = new PaletteButton(-1, &s);
    pb->setTopLeftPosition(10, 20);
    pb->setSize(30 , 30);
    addAndMakeVisible(pb);
    
    int bx = 14, by = 80, bw = 22, bh = 22;
    
    for ( int i = 0; i < palette->getPaletteNumDefaultItems(); i++ )
    {
        PaletteButton *pb = new PaletteButton(i, palette->getPaletteDefaultItem(i));
//        bx = Random().nextFloat()*20;
        pb->setTopLeftPosition(bx, by += 28);
        pb->setSize(bw , bh);
        addAndMakeVisible(pb);
    }
    
    for ( int i = 0 ; i < palette->getPaletteNumUserItems() ; i++ )
    {
        PaletteButton *pb = new PaletteButton( i + palette->getPaletteNumDefaultItems(), palette->getPaletteUserItem(i));
//        bx = Random().nextFloat()*20;
        pb->setTopLeftPosition(bx, by += 28);
        pb->setSize(bw , bh);
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


