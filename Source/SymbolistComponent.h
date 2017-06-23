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
    
    virtual String getSymbolTypeStr() const { return " ??? " ; } ; // every component defines its type

    virtual PageComponent* getPageComponent();
    virtual SymbolistHandler* getSymbolistHandler();
    SymbolistMainComponent* getMainComponent();
    
    UI_EditType getMainEditMode();
    UI_DrawType getMainDrawMode();
    virtual void selectComponent();
    virtual void deselectComponent();
    
    const size_t    getNumSubcomponents( );
    SymbolistComponent*  getSubcomponent( int i );
    void            addSubcomponent( SymbolistComponent *c );
    virtual void    removeSubcomponent( SymbolistComponent *c );
    virtual void    clearAllSubcomponents();
    
    Point<int> positionRelativeTo(SymbolistComponent* to);
    


protected :
    
    bool            is_selected = false;
    Array<SymbolistComponent*>     subcomponents;
    
};

#endif
