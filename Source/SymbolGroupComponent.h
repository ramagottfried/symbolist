//
//  GroupComponent.hpp
//  symbolist
//
//  Created by Jean Bresson on 04/06/2017.
//
//

#ifndef SymbolGroupComponent_h
#define SymbolGroupComponent_h

#include "ScoreComponent.h"
#include "BaseComponent.h"

class SymbolGroupComponent : public ScoreComponent, public BaseComponent
{
    
    public:
    
    SymbolGroupComponent(float x, float y,
                         float w = 10, float h = 10 ) :
        BaseComponent("group" , x , y , w , h ) {};
    
    
    ~SymbolGroupComponent()
    {
        printf("freeing group %p\n", this);
    }
    

    size_t          getNumSubcomponents( );
    BaseComponent*  getSubcomponent( int i );
    void            addSubcomponent( BaseComponent *c );

    int addSymbolMessages(const String &base_address ) override ;

    private :
    
    std::vector<BaseComponent*>     subcomponents;
    
} ;


#endif
