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

class SymbolistComponent : public Component
{
public:
    
    virtual SymbolistComponent* getPageComponent();
    virtual SymbolistComponent* getMainComponent();
    
    UI_EditType getMainEditMode();
};

#endif /* SymbolistComponent_hpp */
