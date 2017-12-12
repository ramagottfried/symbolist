
#include "SymbolistMainComponent.h"
#include "SymbolistMainWindow.h"


SymbolistMainComponent::SymbolistMainComponent(SymbolistHandler *sh)
{
    // score = std::unique_ptr<Score>(new Score());
    std::cout << "MainComponent " << this << std::endl;
    setComponentID("MainComponent");
    
    symbolist_handler = sh;
    
    setLookAndFeel( &look_and_feel );

    
    updatePaletteView();
    
    score_viewport.setViewedComponent( &scoreView, false );
    score_viewport.setFocusContainer (true);
    score_viewport.setScrollBarsShown(true, true);
    
    scoreView.setSize(6000, 2000);
    addAndMakeVisible(score_viewport);
    
    paletteView.selectPaletteButton(-1);
    addAndMakeVisible(paletteView);
    addAndMakeVisible(mouseModeView);
    addChildComponent(timeDisplayView);
    
    addAndMakeVisible(menu);
    menu_h = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
    
    inspector = new PropertyPanelTabs(sh);

    
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
    
    

}


SymbolistMainComponent::~SymbolistMainComponent()
{
    setLookAndFeel(nullptr);
    
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
    timeDisplayView.setBounds(palette_w, menu_h+2, 12, 13);
    
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
    menu.getAllCommands(commands);
}

void SymbolistMainComponent::getCommandInfo (CommandID commandID, ApplicationCommandInfo& result)
{
    menu.getCommandInfo(commandID, result);
}

bool SymbolistMainComponent::perform (const InvocationInfo& info)
{
    return menu.perform(info);
}





