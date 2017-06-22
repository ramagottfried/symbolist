//
//  SymbolistComponent.hpp
//  symbolist
//
//  Created by Jean Bresson on 28/05/2017.
//
//

#ifndef SymbolistComponent_h
#define SymbolistComponent_h

#include "../JuceLibraryCode/JuceHeader.h"
#include "types.h"
#include "SymbolistHandler.h"

class PageComponent; // forward declaration of subclass

class SymbolistComponent : public Component
{
public:
    
    virtual PageComponent* getPageComponent();
    virtual SymbolistHandler* getSymbolistHandler();
    SymbolistMainComponent* getMainComponent();
    UI_EditType getMainEditMode();
    UI_DrawType getMainDrawMode();
    
    Point<int> positionRelativeTo(SymbolistComponent* to)
    {
        if (to == getParentComponent() ) return getPosition() ;
        else return getPosition() + ((SymbolistComponent*)getParentComponent())->positionRelativeTo(to);
    }
};

#endif
