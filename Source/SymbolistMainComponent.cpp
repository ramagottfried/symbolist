
#include "SymbolistMainComponent.h"
#include "SymbolistMainWindow.h"


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
    addAndMakeVisible(mouseModeView);

    addAndMakeVisible(menu);
    
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
    
    
    paletteView.setBounds( 0, 0, 50, h );
    score_viewport.setBounds( 50, 100, w-50, h );
    mouseModeView.setBounds( 50, h-25, w-50, 25 );
    menu.setBounds(50, 0, w-50, 100 );
    
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

Rectangle<float> SymbolistMainComponent::getZoomedRect()
{
    return score_viewport.getViewArea().toFloat() / m_zoom;
}

Rectangle<float> SymbolistMainComponent::getViewRect()
{
    return score_viewport.getViewArea().toFloat();
}


/***************************
 * edit/drax modes
 ***************************/

void SymbolistMainComponent::setMouseMode( UI_EditType m )
{
    mouse_mode = m;
    mouseModeView.setMouseMode( m );
    
    //scoreView.repaint();
}

UI_EditType SymbolistMainComponent::getMouseMode()
{
    return mouse_mode ;
}

void SymbolistMainComponent::setDrawMode( UI_DrawType m )
{
    draw_mode = m;
    mouseModeView.setDrawMode(m);
    //scoreView.repaint();
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
    else if (   key == KeyPress('-') ) { zoom( -0.1 ); }
    else if (   key == KeyPress('=') ) { zoom(  0.1) ; }
    
    return true;
}




//==============================================================================
// The following methods implement the ApplicationCommandTarget interface, allowing
// this window to publish a set of actions it can perform, and which can be mapped
// onto menus, keypresses, etc.

ApplicationCommandTarget* SymbolistMainComponent::getNextCommandTarget()
{
    // this will return the next parent component that is an ApplicationCommandTarget (in this
    // case, there probably isn't one, but it's best to use this method in your own apps).
    return findFirstTargetParentComponent();
}

void SymbolistMainComponent::getAllCommands (Array<CommandID>& commands)
{
    //cout << "SymbolistMainComponent::getAllCommands " << endl;
    
    // this returns the set of all commands that this target can perform..
    const CommandID ids[] = {
        cmd_group,
        cmd_ungroup,
        cmd_deleteSelected,
        cmd_toggleInspector,
        cmd_addToPalette,
        cmd_copy,
        cmd_paste
    };
    
    commands.addArray (ids, numElementsInArray (ids));
}

void SymbolistMainComponent::getCommandInfo (CommandID commandID, ApplicationCommandInfo& result)
{
    //cout << "SymbolistMainComponent::getCommandInfo " << endl;

    const String generalCategory ("General");
    const String demosCategory ("Edit");
    
    switch (commandID)
    {
        case cmd_group:
            result.setInfo ("Group Selected", "Shows the previous demo in the list", demosCategory, 0);
            result.addDefaultKeypress ('g', ModifierKeys::commandModifier);
            break;
            
        case cmd_ungroup:
            result.setInfo ("Ungroup Selected", "Shows the next demo in the list", demosCategory, 0);
            result.addDefaultKeypress ('u', ModifierKeys::commandModifier);
            break;
            
        case cmd_deleteSelected:
            result.setInfo ("Delete Selected", "Shows the 'Welcome' demo", demosCategory, 0);
            result.addDefaultKeypress ( KeyPress::deleteKey, NULL );
            break;
            
        case cmd_toggleInspector:
            result.setInfo ("Inspector", "Shows the 'Animation' demo", demosCategory, 0);
            result.addDefaultKeypress ('i', NULL );
            break;
            
        case cmd_addToPalette:
            result.setInfo ("Add Selected To Palette", "Shows the next demo in the list", demosCategory, 0);
            result.addDefaultKeypress ('w', NULL );
            break;
            
        case cmd_copy:
            result.setInfo ("Copy to clipboard", "Shows the 'Welcome' demo", demosCategory, 0);
            result.addDefaultKeypress ( 'c', ModifierKeys::commandModifier );
            break;
            
        case cmd_paste:
            result.setInfo ("Paste from clipboard", "Shows the 'Animation' demo", demosCategory, 0);
            result.addDefaultKeypress ('v', ModifierKeys::commandModifier );
            break;
            
        default:
            result.setInfo ("no idea!", "does something!", demosCategory, 0);

            break;
    }
}

bool SymbolistMainComponent::perform (const InvocationInfo& info)
{
  //  if ( getMainComponent() )
    {
        switch (info.commandID)
        {
                
            case cmd_group:
                scoreView.getEditedComponent()->groupSelectedSymbols();
                break;
                
            case cmd_ungroup:
                scoreView.getEditedComponent()->ungroupSelectedSymbols();
                break;
                
            case cmd_deleteSelected:
                scoreView.getEditedComponent()->deleteSelectedComponents();
                break;
                
            case cmd_toggleInspector:
                toggleInspector();
                break;
                
            case cmd_addToPalette:
                scoreView.getEditedComponent()->addSelectedSymbolsToPalette();
                break;
                
            case cmd_copy:
                symbolist_handler->copySelectedToClipBoard();
                break;
                
            case cmd_paste:
                symbolist_handler->newFromClipBoard();
                break;
                
            default:
                return false;
        }
    }
    
    return true;
}





