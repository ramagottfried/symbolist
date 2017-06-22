
#include "SymbolistMainComponent.h"

SymbolistMainComponent::SymbolistMainComponent(SymbolistHandler *sh)
{
    // score = std::unique_ptr<Score>(new Score());
    //std::cout << "MainComponent " << this << std::endl;
    setComponentID("MainComponent");
    setSize (600, 400);
    
    symbolist_handler = sh;
    
    paletteView.buildFromPalette(symbolist_handler->getSymbolPalette());
    
    paletteView.selectPaletteButton(0);
    addAndMakeVisible(scoreView);
    addAndMakeVisible(paletteView);
    
    // the main component will receive key events from the subviews
    paletteView.addKeyListener(this);
    scoreView.addKeyListener(this);
    setWantsKeyboardFocus(true);
    addKeyListener(this);
}


SymbolistMainComponent::~SymbolistMainComponent() {}


void SymbolistMainComponent::resized()
{
    scoreView.setBounds( 50, 0, getWidth(), getHeight() );
    paletteView.setBounds( 0, 0, 50, getHeight() );
}

/***************************
 * edit/drax modes
 ***************************/

void SymbolistMainComponent::setEditMode( UI_EditType m )
{
    mouse_mode = m;
    scoreView.repaint();
}

UI_EditType SymbolistMainComponent::getEditMode()
{
    return mouse_mode ;
}

void SymbolistMainComponent::setDrawMode( UI_DrawType m )
{
    draw_mode = m;
    scoreView.repaint();
}

UI_DrawType SymbolistMainComponent::getDrawMode()
{
    return draw_mode ;
}


void SymbolistMainComponent::modifierKeysChanged (const ModifierKeys& modifiers)
{
    if ( modifiers.isCommandDown() )
    {
        setEditMode( UI_EditType::draw );
    }
    else
    {
        setEditMode( UI_EditType::selection );
    }
}

/***************************
 * main hub for key actions
 ***************************/
bool SymbolistMainComponent::keyPressed (const KeyPress& key, Component* originatingComponent)
{
    String desc = key.getTextDescription();
    std::cout << "keyPressed: " << desc << std::endl;;
    
    if( desc == "command + G" ) { scoreView.getEditedComponent()->groupSelectedSymbols(); }
    
    else if( desc == "command + U" ) { scoreView.getEditedComponent()->ungroupSelectedSymbols(); }
    
    else if ( desc == "backspace" ) { scoreView.getEditedComponent()->deleteSelectedSymbols(); }
    
    else if ( desc == "C") {
        symbolist_handler->setCurrentSymbol(0);
        paletteView.selectPaletteButton(0);
    }
    
    else if ( desc == "P") {
        symbolist_handler->setCurrentSymbol(1);
        paletteView.selectPaletteButton(1);
    }
    
    else if ( desc == "option + V") { scoreView.getEditedComponent()->flipSelectedSymbols(0); }
    
    else if ( desc == "option + H") { scoreView.getEditedComponent()->flipSelectedSymbols(1); }
    
    else if ( desc == "spacebar") { symbolist_handler->executeTransportCallback(1); }
    
    else if ( desc == "escape") {
 
        scoreView.getEditedComponent()->unselectAllComponents();
        scoreView.exitEditMode();
        
        symbolist_handler->executeTransportCallback(0); // = stop
        symbolist_handler->symbolistAPI_setTime(0);
        
        scoreView.repaint();
    }
    
    return true;
}










