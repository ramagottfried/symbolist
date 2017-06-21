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
}

PageComponent::~PageComponent() {}

/***************************************************/
/* MODIFICATIONS TO BE TRANSFERRED TO THE SCORE    */
/* will update the data (score) and notify to host environment */
/***************************************************/

void PageComponent::addSymbolComponent( BaseComponent *c )
{
    ScoreComponent::addSymbolComponent( c );
    getSymbolistHandler()->addSymbolToScore( c );
}


void PageComponent::removeSymbolComponent( BaseComponent *c )
{
    getSymbolistHandler()->removeSymbolFromScore( c );
    ScoreComponent::removeSymbolComponent( c );
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
    
    
    if ( getMainEditMode() == UI_EditType::select_mode )
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
