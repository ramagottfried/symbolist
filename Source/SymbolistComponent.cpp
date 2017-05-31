//
//  SymbolistComponent.cpp
//  symbolist
//
//  Created by Jean Bresson on 30/05/2017.
//
//

#include "SymbolistComponent.h"
#include "MainComponent.h"

// recursive methods for easy access to the top-level sore and main component
// from anywhere in the GUI
// redefined in ScoreComponent and MAincomponent
SymbolistComponent* SymbolistComponent::getScoreComponent()
{
    auto p = static_cast<SymbolistComponent*>( getParentComponent() );
    if (p == NULL) return p;
    else return p->getScoreComponent(); // SymbolistMainComponent and ScoreComponent will return the actual ScoreComponent
}


SymbolistComponent* SymbolistComponent::getMainComponent()
{
    auto p = static_cast<SymbolistComponent*>( getParentComponent() );
    if (p == NULL) return p;
    else return p->getMainComponent(); // only a SymbolistMainComponent will return 'this'
}


UI_EditType SymbolistComponent::getMainEditMode()
{
    return static_cast<SymbolistMainComponent*>(getMainComponent())->getEditMode() ;
}
