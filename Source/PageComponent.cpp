
#include "PageComponent.h"
#include "SymbolistMainComponent.h"


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

void PageComponent::addSubcomponent( SymbolistComponent *c )
{
    ScoreComponent::addSubcomponent( c );
    
    if ( ((BaseComponent*)c)->getScoreSymbolPointer() != NULL )
    {
        getSymbolistHandler()->addSymbolToScore( ((BaseComponent*) c) );
    }
}

void PageComponent::removeSubcomponent( SymbolistComponent *c )
{
    if ( ((BaseComponent*)c)->getScoreSymbolPointer() != NULL )
    {
        getSymbolistHandler()->removeSymbolFromScore( ((BaseComponent*) c) );
    }
    
    ScoreComponent::removeSubcomponent( c );
}


void PageComponent::enterStaffSelMode()
{
    exitEditMode();
    
    for( auto c : subcomponents )
    {
        BaseComponent* b = dynamic_cast<BaseComponent*>(c); // << if it's a staff the dynamic_cast prob isn't necessary
        if( b )
        {
            if( b->getSymbolTypeStr() == "staff" )
            {
                b->setStaffSelectionMode( true );
            }
            else
            {
                if( b->componentSelected() )
                {
                    b->setStaffSelectionMode( true );
                }
                else
                {
                    b->setStaffSelectionMode( false );
                    b->setVisible(false);

                }
            }
        }
       
    }
    
    display_mode = staff;
}

void PageComponent::exitStaffSelMode()
{

    for( auto c : subcomponents )
    {
        BaseComponent* b = dynamic_cast<BaseComponent*>(c);
        if( b )
        {
            b->setStaffSelectionMode( false );
            b->setVisible(true);
        }
    }
    
    display_mode = main;

}

vector<BaseComponent*> PageComponent::getSubcomponentsByStaff( String& staff_name )
{
    vector<BaseComponent*> objects;
    Symbol *s = NULL;
    for( int i = 0; i < subcomponents.size(); i++ )
    {
        BaseComponent *c = dynamic_cast<BaseComponent*>(subcomponents[i]);
        if( c )
        {
            s = c->getScoreSymbolPointer();
            
            if( s->getSaff() == staff_name )
                objects.emplace_back( c );
        }
    }
    
    return objects;
}


void PageComponent::enterEditMode( BaseComponent* c )
{
    exitEditMode();
    unselectAllComponents();
    edited_component = c;
    edited_component->setEditMode(true);
    edited_component->toFront(true);
    edited_component->recursiveMaximizeBounds();
    
    display_mode = edit;

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
    
    display_mode = main;

}


ScoreComponent* PageComponent::getEditedComponent()
{
    if ( edited_component == NULL )
        return this;
    else
        return edited_component;
}


void PageComponent::resized ()
{
    if ( edited_component != NULL)
    {
        edited_component->recursiveMaximizeBounds();
    }
    
    score_cursor.setBounds( score_cursor.getPlayPoint() * 100, 0, 50, getHeight() );
}


/************************/
/* Draws the score page */
/************************/

void PageComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );
/*
    auto visibleBounds = getMainComponent()->getViewRect() ;
    String timestr = " t = ";
    timestr += (String) (getSymbolistHandler()->getCurrentTime()) ;
    g.drawText (timestr, visibleBounds , Justification::topLeft, false);
  */
}
