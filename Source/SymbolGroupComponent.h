//
//  GroupComponent.hpp
//  symbolist
//
//  Created by Jean Bresson on 04/06/2017.
//
//

#ifndef SymbolGroupComponent_h
#define SymbolGroupComponent_h

#include "BaseComponent.h"
#include "Symbol.h"

class SymbolGroupComponent : public BaseComponent
{
    
public:
    
    SymbolGroupComponent() = default;
    ~SymbolGroupComponent() = default;
    
    virtual String getSymbolTypeStr() const override { return "group"; }
    int  addSymbolMessages(Symbol* s, const String &base_address ) override ;
    void importFromSymbol( const Symbol &s ) override;

    void selectComponent() override;
    void deselectComponent() override;
    void setEditMode( bool val ) override;
    void paint ( Graphics& g ) override;
    bool intersectRect( Rectangle<int> rect) override;
    
    void updateRelativeAttributes() override;

private :
    
};


#endif
