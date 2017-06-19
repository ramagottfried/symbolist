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
    
    virtual String getSymbolTypeStr() const override { return "group"; }
    int addSymbolMessages(Symbol* s, const String &base_address ) override ;
    void importFromSymbol( const Symbol &s ) override;

    void selectComponent() override;
    void deselectComponent() override;
    
    void mouseDoubleClick(const MouseEvent& event) override;
    void mouseDown(const MouseEvent& event) override;
    void mouseUp(const MouseEvent& event) override;
    void mouseDrag(const MouseEvent& event) override;
    
    void paint ( Graphics& g ) override;
    
    
private :
    
};


#endif
