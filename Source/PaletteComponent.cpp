#include "PaletteComponent.h"
#include "PaletteButton.hpp"
#include "SymbolistMainComponent.h"

using namespace std ;

/********************
 * PALETTE VIEW
 ********************/

PaletteComponent::PaletteComponent()
{
    setComponentID("PaletteComponent");
}

PaletteComponent::~PaletteComponent()
{
    // cout << "deleting all children of palette " << this << endl;
    deleteAllChildren();
}

void PaletteComponent::buildFromPalette(Palette* palette)
{
    palette_pointer = palette;
    
    deleteAllChildren();
    
    // start center points
    int b_cX = 25, b_cY = 30;
    
    // size for default button
    int d_bh = 30, d_bw = 30;
    int d_y_spacing = d_bh + 5;
    
    // size for user buttons
    int bw = 23, bh = 23;
    int b_y_spacing = bh + 5;
    
    int y_separator = 5; // gap between default and user buttons
    
    
    // default "draw" buttons (only one for now)
    {
        Symbol s;
        s.setTypeXYWH("path", 0, 0, d_bh, d_bw);
        s.addMessage("/num_sub_paths", 1 );
        s.addMessage("/path/0/str", "m 4. 4. c 14. 2. 22. 8. 16. 14. c 12. 20. 14. 24. 20. 22.");
        
        PaletteButton *pb = new PaletteButton(-1, &s);
        pb->setSize(d_bh , d_bw);
        pb->setCentrePosition(b_cX, b_cY);
        
        addAndMakeVisible(pb);
        b_cY += d_y_spacing;
    }
    
    // separator
    b_cY += y_separator;
    
    for (int i = 0; i < palette->getPaletteNumDefaultItems(); i++)
    {
        PaletteButton *pb = new PaletteButton(i, palette->getPaletteDefaultItem(i));

        pb->setSize(bw , bh);
        pb->setCentrePosition(b_cX, b_cY);
        addAndMakeVisible(pb);
        b_cY += b_y_spacing;

    }
    
    for (int i = 0; i < palette->getPaletteNumUserItems(); i++)
    {
        PaletteButton *pb = new PaletteButton(i + palette->getPaletteNumDefaultItems(), palette->getPaletteUserItem(i));
        pb->setSize(bw , bh);
        pb->setCentrePosition( b_cX, b_cY);
        addAndMakeVisible(pb);
        b_cY += b_y_spacing;
    }
}


void PaletteComponent::selectPaletteButton(int i)
{
    for (int j = 0; j < getNumChildComponents(); j++)
    {
        PaletteButton *button = dynamic_cast<PaletteButton*>(getChildComponent(j));
        
        // Checks the downcast result.
        if (button != NULL) {
            if (button->getID() == i)
                button->setSelected(true);
            else button->setSelected(false);
        }
        
    }
    
    if ( i >= 0) palette_pointer->setSelectedItem(i);
    
    SymbolistMainComponent* smc = dynamic_cast<SymbolistMainComponent*>(getParentComponent());
    
    // Checks the downcast result.
    if (smc != NULL)
    {
        if (i >= 0)
            smc->setDrawMode(UI_DrawType::FROM_TEMPLATE);
        // specialModes
        else
            smc->setDrawMode(UI_DrawType::FREE_DRAW);
    }
    repaint();
}


void PaletteComponent::paint (Graphics& g)
{
    g.fillAll( Colours::white );
    g.setColour(Colours::lightgrey);
    g.drawLine(getWidth(), 0, getWidth(), getHeight());
}


