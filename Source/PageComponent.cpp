//
//  PageComponent.cpp
//  symbolist
//
//  Created by Jean Bresson on 04/06/2017.
//
//

#include "PageComponent.h"
#include "MainComponent.h"


PageComponent::PageComponent()
{
    setComponentID("PageComponent");
    activateLasso();
    //std::cout << "PageComponent " << this << std::endl;
}


/***************************************************/
/* MODIFICATIONS TO BE TRANSFERRED TO THE SCORE    */
/* will update the data (score) and notify to host environment */
/***************************************************/

void PageComponent::addSymbolComponent( BaseComponent *c )
{
    ScoreComponent::addSymbolComponent( c );
    ((SymbolistMainComponent*) (getMainComponent()))->addSymbolToScore( c );
}


void PageComponent::removeSymbolComponent( BaseComponent *c )
{
    ((SymbolistMainComponent*) (getMainComponent()))->removeSymbolFromScore( c );
    ScoreComponent::removeSymbolComponent( c );
}


void PageComponent::enterEditMode( BaseComponent* c )
{
    edited_component = c;
    edited_component->toFront(true);
    edited_component->setEditMode(true);
    edited_component->recursiveMaximizeBounds();
    this->deactivateLasso();
    edited_component->activateLasso();
    
}

void PageComponent::exitEditMode( )
{
    if ( edited_component != NULL )
    {
        edited_component->recursiveShrinkBounds();
        edited_component->setEditMode(false);
        edited_component->deactivateLasso();
        this->activateLasso();
        edited_component = NULL;
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
    
    String msg = "";
    String timestr = "t = ";
    timestr += ((String) ((SymbolistMainComponent*) (getMainComponent()))->getCurrentTime()) ;
    
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
            Symbol *s = static_cast<SymbolistMainComponent*>( getMainComponent() )->getCurrentSymbol();
            msg += s->getOSCMessageValue(s->getOSCMessagePos("/type")).getString();
            break;
        }
        case UI_EditType::draw_alt_mode:
        {
            msg += " draw alter: " ;
            Symbol *s = static_cast<SymbolistMainComponent*>( getMainComponent() )->getCurrentSymbol();
            msg += s->getOSCMessageValue(s->getOSCMessagePos("/type")).getString();
            break;
        }
    }
    
    g.drawText (msg, getLocalBounds() , Justification::bottom, false);
    g.drawText (timestr, getLocalBounds() , Justification::topLeft, false);
  
}
