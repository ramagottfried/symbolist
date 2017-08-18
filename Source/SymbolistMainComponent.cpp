
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
    menu_h = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
    
    inspector = new SymbolPropertiesPanel(sh);

    
    /*
    addAndMakeVisible(score_cursor);
    addAndMakeVisible(time_pointGUI);
    */
    
    
    // the main component will receive key events from the subviews
    //paletteView.addKeyListener(this);
    //scoreView.addKeyListener(this);
    
    //setWantsKeyboardFocus(true);
    
    //addKeyListener(this);
    
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
    
    menu_h = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
    
    paletteView.setBounds( 0, 0, palette_w, h );
    score_viewport.setBounds( palette_w, menu_h, w-palette_w, h );
    mouseModeView.setBounds( palette_w, h-25, w-palette_w, 25 );
    menu.setBounds(palette_w, 0, w-palette_w, menu_h );
    
    if( inspector->isVisible() )
    {
        inspector->setSize(400, h - score_viewport.getScrollBarThickness() - menu_h);
        inspector->setTopRightPosition( w - score_viewport.getScrollBarThickness(), menu_h );
    }
    
//    score_cursor.setBounds( score_cursor.getPlayPoint() * 100, menu_h, 50, getHeight()-menu_h );
//    time_pointGUI.setBounds(palette_w, getBottom() - 50, getWidth()-palette_w, 50);
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
        
        auto menuH = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
        inspector->setSize(400, getHeight() - score_viewport.getScrollBarThickness() - menuH);
        inspector->setTopRightPosition( getWidth() - score_viewport.getScrollBarThickness(), menuH );
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
 * key actions (now handled by ApplicationCommand Target to synchronize with menu)
 ***************************/


ApplicationCommandTarget* SymbolistMainComponent::getNextCommandTarget()
{
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
        cmd_paste,
        cmd_flipH,
        cmd_flipV,
        cmd_zoomIn,
        cmd_zoomOut,
        cmd_esc,
        cmd_playmsg
    };
    
    commands.addArray (ids, numElementsInArray (ids));
}

void SymbolistMainComponent::getCommandInfo (CommandID commandID, ApplicationCommandInfo& result)
{
    //cout << "SymbolistMainComponent::getCommandInfo " << endl;

   // const String generalCategory ("View");
    //const String demosCategory ("Edit");
    
    switch (commandID)
    {
        case cmd_group:
            result.setInfo ("group selected", String(), String(), 0);
            result.addDefaultKeypress ('g', ModifierKeys::commandModifier);
            break;
            
        case cmd_ungroup:
            result.setInfo ("ungroup selected", String(), String(), 0);
            result.addDefaultKeypress ('u', ModifierKeys::commandModifier);
            break;
            
        case cmd_deleteSelected:
            result.setInfo ("delete selected", String(), String(), 0);
            result.addDefaultKeypress ( KeyPress::backspaceKey, ModifierKeys::noModifiers );
            break;
            
        case cmd_toggleInspector:
            result.setInfo ("open/close inspector", String(), String(), 0);
            result.addDefaultKeypress ('i', ModifierKeys::noModifiers );
            break;
            
        case cmd_addToPalette:
            result.setInfo ("add selected To palette", String(), String(), 0);
            result.addDefaultKeypress ('w', ModifierKeys::noModifiers );
            break;
            
        case cmd_copy:
            result.setInfo ("copy", String(),String(), 0);
            result.addDefaultKeypress ( 'c', ModifierKeys::commandModifier );
            break;
            
        case cmd_paste:
            result.setInfo ("paste", String(),String(), 0);
            result.addDefaultKeypress ('v', ModifierKeys::commandModifier );
            break;
            
        case cmd_flipH:
            result.setInfo ("flip horizontally",String(),String(), 0);
            result.addDefaultKeypress ('h', ModifierKeys::altModifier );
            break;
            
        case cmd_flipV:
            result.setInfo ("flip vertically", String(),String(), 0);
            result.addDefaultKeypress ('v', ModifierKeys::altModifier );
            break;
            
        case cmd_zoomIn:
            result.setInfo ("zoom in", String(),String(), 0);
            result.addDefaultKeypress ( '=', ModifierKeys::noModifiers );
            break;
            
        case cmd_zoomOut:
            result.setInfo ("zoom out", String(),String(), 0);
            result.addDefaultKeypress ('-',  ModifierKeys::noModifiers );
            break;
            
        case cmd_esc:
            result.setInfo ("deselect", String(), String(), 0);
            result.addDefaultKeypress (KeyPress::escapeKey,  ModifierKeys::noModifiers );
            break;
            
        case cmd_playmsg:
            result.setInfo ("send transport start message", String(), String(), 0);
            result.addDefaultKeypress (KeyPress::spaceKey,  ModifierKeys::noModifiers );
            break;
            
        default:
            result.setInfo ("undefined", "", "", 0);

            break;
    }
    
    
    /*
     // Palette selection not currently in use
    else if (   key == KeyPress('c')) { // would be better to type a number and that selects the nth palete item..
        symbolist_handler->setCurrentSymbol(0);
        paletteView.selectPaletteButton(0);
    }
    
    else if (   key == KeyPress('p') ) {
        symbolist_handler->setCurrentSymbol(1);
        paletteView.selectPaletteButton(1);
    }
     */
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
                
            case cmd_flipH:
                scoreView.getEditedComponent()->flipSelectedSymbols(1);
                break;
                
            case cmd_flipV:
                scoreView.getEditedComponent()->flipSelectedSymbols(0);
                break;
                
            case cmd_zoomIn:
                zoom( 0.1 );
                break;
                
            case cmd_zoomOut:
                zoom( -0.1 );
                break;

            case cmd_esc:
                scoreView.getEditedComponent()->unselectAllComponents();
                scoreView.exitEditMode();
                
                symbolist_handler->executeTransportCallback(0); // = stop
                symbolist_handler->symbolistAPI_setTime(0);
                symbolist_handler->clearInspector();
                scoreView.repaint();
                break;
                
            case cmd_playmsg:
                symbolist_handler->executeTransportCallback(1);
                break;
                
            default:
                return false;
        }
    }
    
    return true;
}





