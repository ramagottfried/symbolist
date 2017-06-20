//
//  SymbolistComponent.cpp
//  symbolist
//
//  Created by Jean Bresson on 30/05/2017.
//
//

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
  return (SymbolistMainComponent*)getPageComponent()->getParentComponent() ;
}


UI_EditType SymbolistComponent::getMainEditMode()
{
    if ( getMainComponent() != NULL)
    {
        return getMainComponent()->getEditMode() ;
    }
    else
    {
        std::cout << "Warning: trying to get the edit_mode => MainComponent not found.." << std::endl;
        return UI_EditType::select_mode;
    }
}

