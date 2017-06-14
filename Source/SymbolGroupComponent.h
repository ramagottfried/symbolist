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

class SymbolGroupComponent : public BaseComponent
{
    
public:
    
    SymbolGroupComponent( const Symbol& s );
    ~SymbolGroupComponent();
    
    String getSymbolTypeStr() const override { return "group"; }

    void paint ( Graphics& g ) override;

    int addSymbolMessages(Symbol* s, const String &base_address ) override ;
    void importFromSymbol( const Symbol &s ) override;

private :
    
};


#endif
