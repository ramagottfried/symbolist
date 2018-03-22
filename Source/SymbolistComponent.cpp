#include "SymbolistComponent.h"
#include "SymbolistMainComponent.h"

// recursive methods for easy access to the top-level score and main component
// from anywhere in the GUI
// redefined in PageComponent and MAincomponent
PageComponent* SymbolistComponent::getPageComponent()
{
    PageComponent* pc = findParentComponentOfClass<PageComponent>();
    return pc;
}


SymbolistHandler* SymbolistComponent::getSymbolistHandler()
{
    SymbolistMainComponent* mc = findParentComponentOfClass<SymbolistMainComponent>() ;
    if( mc )
        return mc->getSymbolistHandler();
    else
        return nullptr;
}

SymbolistMainComponent* SymbolistComponent::getMainComponent()
{
    SymbolistMainComponent* mc = findParentComponentOfClass<SymbolistMainComponent>() ;
    return mc;
}



/*****************************
 * Management of sucomponents
 * Add/remove operations apply on views only
 *****************************/

const size_t SymbolistComponent::getNumSubcomponents()
{
    return subcomponents.size() ;
}

SymbolistComponent* SymbolistComponent::getSubcomponentByID( String& id )
{
    
    for( int i = 0; i < subcomponents.size(); i++ )
    {
        if( subcomponents[i]->getComponentID() == id )
            return subcomponents[i];
    }
    
    return nullptr;
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




UI_EditType SymbolistComponent::getMainMouseMode()
{
    if ( getMainComponent() != NULL)
    {
        return getMainComponent()->getMouseMode() ;
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
    SymbolistComponent* parentComponent = dynamic_cast<SymbolistComponent*>(getParentComponent());
    if (to == getParentComponent())
        return getPosition();
    
    // Checks downcast result.
    else if(parentComponent != NULL)
        return getPosition() + parentComponent->positionRelativeTo(to);
    
    return Point<int>(0, 0);
}

bool SymbolistComponent::intersectRect( Rectangle<int> rect)
{
    return getBounds().intersects(rect);
}

// basic selection mechanism
void SymbolistComponent::mouseDownSelection( const MouseEvent& event )
{
    ScoreComponent* parent = dynamic_cast<ScoreComponent*>(getParentComponent());
    
    // Checks downcast exception.
    if (parent != NULL)
    {
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
    
}




