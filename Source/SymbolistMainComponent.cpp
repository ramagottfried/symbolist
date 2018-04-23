#include "SymbolistMainComponent.h"
#include "SymbolistMainWindow.h"

SymbolistMainComponent::SymbolistMainComponent(SymbolistHandler* mainController)
{
    std::cout << __func__ << " " << this << std::endl;
    setComponentID("MainComponent");

    /* Sets the corresponding controllers for this
     * instance of SymbolistMainComponent and all
     * its child component.
     */
    setController(mainController);
    palette_view.setController(getController()->getPaletteController());
    score_view.setController(getController()->getPageController());
    
    /*
     * Sets model for this SymbolistMainComponent instance
     * and all its child components.
     */
    setModel(getController()->getModel());
    palette_view.setModel(getModel());
    score_view.setModel(getModel());
    
    /* Adds this SymbolistMainComponent instance and
     * its child components as observers of the model.
     */
    getModel()->attach(this);
    getModel()->attach(&palette_view);
    getModel()->attach(&score_view);
    
    // Sets UI look and creates the palette buttons.
    setLookAndFeel(&look_and_feel);
    updatePaletteView();
    
    // Sets scoreView properties and makes it visible.
    score_viewport.setViewedComponent(&score_view, false);
    score_viewport.setFocusContainer (true);
    score_viewport.setScrollBarsShown(true, true);
    
    score_view.setSize(6000, 2000);
    addAndMakeVisible(score_viewport);
    
    /* Highlights the default selected
     * button on the palette.
     */
    palette_view.selectPaletteButton(-1);
    
    // Makes all the other views visible.
    addAndMakeVisible(palette_view);
    addAndMakeVisible(mouse_mode_view);
    addChildComponent(time_display_view);
    addAndMakeVisible(menu);
    menu_h = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
    
    inspector = new PropertyPanelTabs(mainController);
    
    setSize(600, 400);
}

SymbolistMainComponent::~SymbolistMainComponent()
{
    assert(getController() != NULL);
    setLookAndFeel(nullptr);
    
    palette_view.deleteAllChildren();
    inspector->removeAllChildren();
    
    /* Removes SymbolistMainComponent and its child components
     * from the SymbolistModel's observers list.
     */
	getModel()->detach(&palette_view);
    getModel()->detach(&score_view);
    getModel()->detach(this);
}

/***********************************
 *       PALETTE VIEW METHODS      *
 ***********************************/
void SymbolistMainComponent::updatePaletteView()
{
    palette_view.buildFromPalette();
}

void SymbolistMainComponent::addSelectedSymbolsToPalette()
{
    palette_view.addSymbolsToPalette(score_view.getSelectedItems());
    updatePaletteView();
}

/***********************************
 *        SCORE VIEW METHODS       *
 ***********************************/

void SymbolistMainComponent::groupSelectedSymbols()
{
	score_view.getEditedComponent()->groupSelectedSymbols();
}

void SymbolistMainComponent::ungroupSelectedSymbols()
{
	score_view.getEditedComponent()->ungroupSelectedSymbols();
}

/***************************************
 *        INSPECTOR VIEW METHODS       *
 ***************************************/

void SymbolistMainComponent::toggleInspector()
{
    if( !inspector->isVisible() )
    {
        auto selectedItems = getPageComponent()->getSelectedItems();
        inspector->setInspectorObject( dynamic_cast<BaseComponent*>(selectedItems.getLast()) );
        
        addAndMakeVisible( inspector );
        
        auto menuH = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
        inspector->setSize(400, getHeight() - score_viewport.getScrollBarThickness() - menuH);
        inspector->setTopRightPosition( getWidth() - score_viewport.getScrollBarThickness(), menuH );
        inspector->resized();
    }
    else
    {
        removeChildComponent(inspector);
        inspector->setVisible(false);
    }
}

void SymbolistMainComponent::resized()
{
    auto w = getWidth();
    auto h = getHeight();
	
    menu_h = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
	
    palette_view.setBounds( 0, 0, palette_w, h );
    score_viewport.setBounds( palette_w, menu_h, w-palette_w, h );
    mouse_mode_view.setBounds( palette_w, h-25, w-palette_w, 25 );
    menu.setBounds(palette_w, 0, w-palette_w, menu_h );
    time_display_view.setBounds(palette_w, menu_h+2, 12, 13);
	
    if( inspector->isVisible() )
    {
        inspector->setSize(400, h - score_viewport.getScrollBarThickness() - menu_h);
        inspector->setTopRightPosition( w - score_viewport.getScrollBarThickness(), menu_h );
    }
	
//    score_cursor.setBounds( score_cursor.getPlayPoint() * 100, menu_h, 50, getHeight()-menu_h );
//    time_pointGUI.setBounds(palette_w, getBottom() - 50, getWidth()-palette_w, 50);
}

void SymbolistMainComponent::zoom( float delta )
{
    m_zoom += delta;
    score_view.setTransform( AffineTransform().scale( m_zoom ) );
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
    mouse_mode_view.setMouseMode( m );
}

UI_EditType SymbolistMainComponent::getMouseMode()
{
    return mouse_mode;
}

void SymbolistMainComponent::setDrawMode(UI_DrawType m)
{
    draw_mode = m;
    mouse_mode_view.setDrawMode(m);
}

UI_DrawType SymbolistMainComponent::getDrawMode()
{
    return draw_mode ;
}

void SymbolistMainComponent::modifierKeysChanged (const ModifierKeys& modifiers)
{
    if (modifiers.isCommandDown())
    {
        setMouseMode(UI_EditType::DRAW);
    }
    else
    {
        setMouseMode(UI_EditType::SELECTION);
    }
    
    current_mods = modifiers;
}

/***********************************************************************************
 * key actions (now handled by ApplicationCommand Target to synchronize with menu) *
 ***********************************************************************************/
ApplicationCommandTarget* SymbolistMainComponent::getNextCommandTarget()
{
    return findFirstTargetParentComponent();
}

void SymbolistMainComponent::getAllCommands(Array<CommandID>& commands)
{
    menu.getAllCommands(commands);
}

void SymbolistMainComponent::getCommandInfo(CommandID commandID, ApplicationCommandInfo& result)
{
    menu.getCommandInfo(commandID, result);
}

bool SymbolistMainComponent::perform (const InvocationInfo& info)
{
    return menu.perform(info);
}





