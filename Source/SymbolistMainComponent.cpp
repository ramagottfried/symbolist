
#include "SymbolistMainComponent.h"

SymbolistMainComponent::SymbolistMainComponent(SymbolistHandler *sh)
{
    // score = std::unique_ptr<Score>(new Score());
    std::cout << "MainComponent " << this << std::endl;
    setComponentID("MainComponent");
    
    symbolist_handler = sh;
    
    updatePaletteView();
    
    score_viewport.setViewedComponent( &scoreView, false );
    score_viewport.setFocusContainer (true);
    score_viewport.setScrollBarsShown(true, true);
    
    scoreView.setSize(6000, 2000);
    addAndMakeVisible(score_viewport);
    
    paletteView.selectPaletteButton(-1);
    addAndMakeVisible(paletteView);
    
    inspector = new SymbolPropertiesPanel(sh);

    // the main component will receive key events from the subviews
    paletteView.addKeyListener(this);
    scoreView.addKeyListener(this);
    setWantsKeyboardFocus(true);
    addKeyListener(this);
    
    setSize (600, 400);
    
    setLookAndFeel( &look_and_feel );

}


SymbolistMainComponent::~SymbolistMainComponent()
{
    paletteView.deleteAllChildren();
    inspector->removeAllChildren();
}


void SymbolistMainComponent::resized()
{
    auto w = getWidth();
    auto h = getHeight();
    
    score_viewport.setBounds (50, 0, w-50, h );
    paletteView.setBounds( 0, 0, 50, h );
    
    if( inspector->isVisible() )
    {
        inspector->setSize(400, h - score_viewport.getScrollBarThickness());
        inspector->setTopRightPosition( w - score_viewport.getScrollBarThickness(), 0 );
    }
}

void SymbolistMainComponent::updatePaletteView()
{
    paletteView.buildFromPalette(symbolist_handler->getSymbolPalette());
}


void SymbolistMainComponent::toggleInspector()
{
    if( !inspector->isVisible() )
    {
        auto sel = getPageComponent()->getSelectedItems();
        inspector->setInspectorObject( (BaseComponent *)sel.getLast() );
        
        addAndMakeVisible(inspector);
        inspector->setSize(400, getHeight() - score_viewport.getScrollBarThickness());
        inspector->setTopRightPosition( getWidth() - score_viewport.getScrollBarThickness(), 0 );
    }
    else
    {
        removeChildComponent(inspector);
        inspector->setVisible(false);
    }
}

void SymbolistMainComponent::zoom( float delta )
{
    m_zoom += delta;
    scoreView.setTransform( AffineTransform().scale( m_zoom ) );
    repaint();
}


/***************************
 * edit/drax modes
 ***************************/

void SymbolistMainComponent::setMouseMode( UI_EditType m )
{
    mouse_mode = m;
    scoreView.repaint();
}

UI_EditType SymbolistMainComponent::getMouseMode()
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
        setMouseMode( UI_EditType::draw );
    }
    else
    {
        setMouseMode( UI_EditType::selection );
    }
    
    current_mods = modifiers;
}

/***************************
 * main hub for key actions
 ***************************/
bool SymbolistMainComponent::keyPressed (const KeyPress& key, Component* originatingComponent)
{
    String desc = key.getTextDescription();
    cout << "keyPressed: " << desc << endl;;
   
    if(         key == KeyPress ('g', ModifierKeys::commandModifier, 0)) { scoreView.getEditedComponent()->groupSelectedSymbols(); }
    
    else if(    key == KeyPress ('u', ModifierKeys::commandModifier, 0) ) { scoreView.getEditedComponent()->ungroupSelectedSymbols(); }
    
    else if (   key == KeyPress::backspaceKey ) { scoreView.getEditedComponent()->deleteSelectedComponents(); }
    
    else if (   key == KeyPress('c')) { // would be better to type a number and that selects the nth palete item..
        symbolist_handler->setCurrentSymbol(0);
        paletteView.selectPaletteButton(0);
    }
    
    else if (   key == KeyPress('p') ) {
        symbolist_handler->setCurrentSymbol(1);
        paletteView.selectPaletteButton(1);
    }
    
    else if (   key == KeyPress ('v', ModifierKeys::altModifier, 0) ) { scoreView.getEditedComponent()->flipSelectedSymbols(0); }
    
    else if (   key == KeyPress ('h', ModifierKeys::altModifier, 0) ) { scoreView.getEditedComponent()->flipSelectedSymbols(1); }
    
    else if (   key == KeyPress::spaceKey ) { symbolist_handler->executeTransportCallback(1); }
    
    else if (   key == KeyPress::escapeKey ) {
 
        scoreView.getEditedComponent()->unselectAllComponents();
        scoreView.exitEditMode();
        
        symbolist_handler->executeTransportCallback(0); // = stop
        symbolist_handler->symbolistAPI_setTime(0);
        
        scoreView.repaint();
    }
    else if (   key == KeyPress ('i') ) { toggleInspector(); }
    else if (   key == KeyPress('w') ) { scoreView.getEditedComponent()->addSelectedSymbolsToPalette(); }

    else if (   key == KeyPress('c', ModifierKeys::commandModifier, 0) ) { symbolist_handler->copySelectedToClipBoard(); }
    else if (   key == KeyPress('v', ModifierKeys::commandModifier, 0) ) { symbolist_handler->newFromClipBoard(); }
    else if (   key == KeyPress('-') ) { zoom( -0.01 ); }
    else if (   key == KeyPress('=') ) { zoom(  0.01) ; }
    
    return true;
}










