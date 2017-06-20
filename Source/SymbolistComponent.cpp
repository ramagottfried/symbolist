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
SymbolistComponent* SymbolistComponent::getPageComponent()
{
    auto p = static_cast<SymbolistComponent*>( getParentComponent() );
    if (p == NULL) return p;
    else return p->getPageComponent(); // SymbolistMainComponent and PageComponent will return the actual PageComponent
}


SymbolistHandler* SymbolistComponent::getSymbolistHandler()
{
    auto p = static_cast<SymbolistComponent*>( getParentComponent() );
    if (p == NULL) return NULL;
    else return p->getSymbolistHandler(); // only a SymbolistMainComponent will return 'something different'
}


UI_EditType SymbolistComponent::getMainEditMode()
{
    SymbolistMainComponent* smc = (SymbolistMainComponent*)getPageComponent()->getParentComponent() ;
    if ( smc != NULL)
        return smc->getEditMode() ;
    else {
        std::cout << "Warning: trying to get the edit_mode => MainComponent not found.." << std::endl;
        return UI_EditType::select_mode;
    }
}
