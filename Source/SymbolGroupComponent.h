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
    
    SymbolGroupComponent( const Symbol& s ) : BaseComponent( s ) {}
    ~SymbolGroupComponent()
    {
        printf("freeing group %p\n", this);
    }
    
    void paint ( Graphics& g ) override;

    int addSymbolMessages(Symbol* s, const String &base_address ) override ;
    void importFromSymbol( ) ;

private :
    
};


#endif
