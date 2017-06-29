

#include "SymbolistComponent.h"
#include "SymbolistMainComponent.h"

// recursive methods for easy access to the top-level sore and main component
// from anywhere in the GUI
// redefined in PageComponent and MAincomponent
PageComponent* SymbolistComponent::getPageComponent()
{
    SymbolistComponent* p = (SymbolistComponent*)getParentComponent() ;
    if (p == NULL) return NULL;
    else return p->getPageComponent(); // SymbolistMainComponent and PageComponent will return the actual PageComponent
}


SymbolistHandler* SymbolistComponent::getSymbolistHandler()
{
    SymbolistComponent* p = (SymbolistComponent*)getParentComponent() ;
    if (p == NULL) return NULL;
    else return p->getSymbolistHandler(); // only a SymbolistMainComponent will return 'something different'
}

SymbolistMainComponent* SymbolistComponent::getMainComponent()
{
    PageComponent* pc = getPageComponent();
    if (pc == NULL) return NULL;
    else return (SymbolistMainComponent*)pc->getParentComponent() ;
}



/*****************************
 * Management of sucomponents
 * Add/remove operations apply on views only
 *****************************/

const size_t SymbolistComponent::getNumSubcomponents()
{
    return subcomponents.size() ;
}

SymbolistComponent* SymbolistComponent::getSubcomponent( int i )
{
    return subcomponents[i] ;
}

void SymbolistComponent::addSubcomponent( SymbolistComponent *c )
{
    subcomponents.add( c );
    addAndMakeVisible( c );
}

void SymbolistComponent::removeSubcomponent( SymbolistComponent *c )
{
    removeChildComponent(c);
    for ( int i = 0; i < subcomponents.size(); i++ )
    {
        if ( subcomponents[i] == c ) subcomponents.remove( i );
    }
}

void SymbolistComponent::clearAllSubcomponents()
{
    for ( int i = 0; i < subcomponents.size(); i++ )
    {
        subcomponents[i]->clearAllSubcomponents();
        removeChildComponent( subcomponents[i] );
        delete subcomponents[i];
    }
    subcomponents.clear();
}




UI_EditType SymbolistComponent::getMainEditMode()
{
    if ( getMainComponent() != NULL)
    {
        return getMainComponent()->getEditMode() ;
    }
    else
    {
        std::cout << "Warning: trying to get the Main Edit Mode => MainComponent not found.." << std::endl;
        return UI_EditType::selection;
    }
}

UI_DrawType SymbolistComponent::getMainDrawMode()
{
    if ( getMainComponent() != NULL)
    {
        return getMainComponent()->getDrawMode() ;
    }
    else
    {
        std::cout << "Warning: trying to get the Main Draw Mode => MainComponent not found.." << std::endl;
        return UI_DrawType::free_draw ;
    }
}

void SymbolistComponent::selectComponent()
{
    is_selected = true;
    repaint();
}

void SymbolistComponent::deselectComponent()
{
    is_selected = false;
    repaint();
}

bool SymbolistComponent::componentSelected()
{
    return is_selected;
}

Point<int> SymbolistComponent::positionRelativeTo(SymbolistComponent* to)
{
    if (to == getParentComponent() ) return getPosition() ;
        else return getPosition() + ((SymbolistComponent*)getParentComponent())->positionRelativeTo(to);
}

bool SymbolistComponent::intersectRect( Rectangle<int> rect)
{
    return getBounds().intersects(rect);
}

// basic selection mechanism
void SymbolistComponent::mouseDownSelection( const MouseEvent& event )
{
    ScoreComponent* parent = (ScoreComponent*)getParentComponent();
    
    if ( event.mods.isShiftDown() )
    {
        if ( componentSelected() )
            parent->removeFromSelection(this);
        else
            parent->addToSelection(this);
        
    }
    else
    {
        if ( ! componentSelected() )
        {
            parent->unselectAllComponents();
            parent->addToSelection(this);
        }
    }
}




