#include "SymbolistComponent.h"
#include "SymbolistMainComponent.h"
#include "symbolist-utils.hpp"

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

UI_EditType SymbolistComponent::getMainMouseMode()
{
    if (getMainComponent() != NULL)
    {
        return getMainComponent()->getMouseMode() ;
    }
    else
    {
        std::cout << "Warning: trying to get the Main Edit Mode => MainComponent not found.." << std::endl;
        return UI_EditType::SELECTION;
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
        return UI_DrawType::FREE_DRAW ;
    }
}










