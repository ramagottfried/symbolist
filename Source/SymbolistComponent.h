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
    
    virtual SymbolistComponent* getScoreComponent();
     /*
      virtual SymbolistComponent* getScoreComponent() {
        auto p = static_cast<SymbolistComponent*>( getParentComponent() );
        if (p == NULL) {
            return p;
        }
        else return p->getScoreComponent();
        
    }
      */
    
    virtual SymbolistComponent* getMainComponent();
    
    UI_EditMode getMainEditMode();
};

#endif /* SymbolistComponent_hpp */
