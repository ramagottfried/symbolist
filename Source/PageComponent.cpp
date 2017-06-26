//
//  PageComponent.cpp
//  symbolist
//
//  Created by Jean Bresson on 04/06/2017.
//
//

#include "PageComponent.h"


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
    edited_component->toFront(true);
    edited_component->recursiveMaximizeBounds();
    edited_component->setEditMode(true);
}


void PageComponent::exitEditMode( )
{
    if ( edited_component != NULL )
    {
        edited_component->recursiveShrinkBounds();
        edited_component->setEditMode(false);
        edited_component->reportModification();
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
    
    
    if ( getMainEditMode() == UI_EditType::selection )
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
    
    g.drawText (msg, getLocalBounds() , Justification::bottom, false);
    
    String timestr = " t = ";
    timestr += (String) (getSymbolistHandler()->getCurrentTime()) ;
    g.drawText (timestr, getLocalBounds() , Justification::topLeft, false);
  
}
