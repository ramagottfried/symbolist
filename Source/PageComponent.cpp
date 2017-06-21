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
    edited_component->setEditMode(true);
    edited_component->recursiveMaximizeBounds();
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
    if ( edited_component != NULL) edited_component->setSize(getWidth(),getHeight());
    
    score_cursor.setBounds( score_cursor.getPlayPoint() * 100, 0, 50, getHeight() );
}


/************************/
/* Draws the score page */
/************************/

void PageComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );

    g.setFont (Font (16.0f));
    g.setColour (Colours::grey);
    
    String msg = "";
    String timestr = "t = ";
    timestr += (String) (getSymbolistHandler()->getCurrentTime()) ;
    
    switch ( getMainEditMode() )
    {
        case UI_EditType::select_mode:
            msg += " select / group / mode" ;
            break;
        case UI_EditType::select_alt_mode:
            msg += " select / resize mode: " ;
            break;
        case UI_EditType::draw_mode:
        {
            msg += " draw mode: " ;
            Symbol *s = getSymbolistHandler()->getCurrentSymbol();
            msg += s->getOSCMessageValue(s->getOSCMessagePos("/type")).getString();
            break;
        }
        case UI_EditType::draw_alt_mode:
        {
            msg += " draw alter: " ;
            Symbol *s = getSymbolistHandler()->getCurrentSymbol();
            msg += s->getOSCMessageValue(s->getOSCMessagePos("/type")).getString();
            break;
        }
    }
    
    g.drawText (msg, getLocalBounds() , Justification::bottom, false);
    g.drawText (timestr, getLocalBounds() , Justification::topLeft, false);
  
}
