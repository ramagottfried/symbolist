
#include "PageComponent.h"
#include "SymbolistMainComponent.h"

PageComponent::PageComponent()
{
    setComponentID("PageComponent");
    edited_component = NULL;
    addAndMakeVisible(score_cursor);
    addAndMakeVisible(time_pointGUI);
}

PageComponent::~PageComponent() {}

/***************************************************/
/* MODIFICATIONS TO BE TRANSFERRED TO THE SCORE    */
/* will update the data (score) and notify to host environment */
/***************************************************/

void PageComponent::addSubcomponent( SymbolistComponent *c )
{
    ScoreComponent::addSubcomponent( c );
    if ( ((BaseComponent*) c)->getScoreSymbolPointer() != NULL )
    {
        getSymbolistHandler()->addSymbolToScore( ((BaseComponent*) c) );
    }
}

void PageComponent::removeSubcomponent( SymbolistComponent *c )
{
    if ( ((BaseComponent*) c)->getScoreSymbolPointer() != NULL )
    {
        getSymbolistHandler()->removeSymbolFromScore( ((BaseComponent*) c) );
    }
    ScoreComponent::removeSubcomponent( c );
}


void PageComponent::enterEditMode( BaseComponent* c )
{
    exitEditMode();
    unselectAllComponents();
    edited_component = c;
    edited_component->setEditMode(true);
    edited_component->toFront(true);
    edited_component->recursiveMaximizeBounds();
}


void PageComponent::exitEditMode( )
{
    if ( edited_component != NULL )
    {
        edited_component->recursiveShrinkBounds();
        edited_component->reportModification();
        // reportModificaiton must be before setting Edit mode to handle case of repositioned sub symbols
        edited_component->setEditMode(false);
        edited_component = NULL;
    }
}


ScoreComponent* PageComponent::getEditedComponent()
{
    if ( edited_component == NULL ) return this;
    else return edited_component;
}


void PageComponent::resized ()
{
    if ( edited_component != NULL)
    {
        edited_component->recursiveMaximizeBounds();
    }
    
    score_cursor.setBounds( score_cursor.getPlayPoint() * 100, 0, 50, getHeight() );
    time_pointGUI.setBounds(0, getBottom() - 50, getWidth(), 50);
}


/************************/
/* Draws the score page */
/************************/

void PageComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );

    g.setFont (Font (16.0f));
    g.setColour (Colours::grey);
    
    String msg;
    
    
    if ( getMainMouseMode() == UI_EditType::selection )
    {
        msg = " select " ;
    }
    else if ( getMainDrawMode() == UI_DrawType::from_template )
    {
        msg = " draw " ;
        msg += getSymbolistHandler()->getCurrentSymbol()->getType();
    }
    else
    {
            msg += " draw lines " ;
    }
    
    auto visibleBounds = getMainComponent()->getViewer()->getViewArea();
    
    g.drawText (msg, visibleBounds , Justification::bottom, false);
    
    String timestr = " t = ";
    timestr += (String) (getSymbolistHandler()->getCurrentTime()) ;
    g.drawText (timestr, visibleBounds , Justification::topLeft, false);
  
}
