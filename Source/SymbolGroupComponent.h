#pragma once

#include "BaseComponent.h"

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
    
    void h_flip() override;
    void v_flip() override;

private :
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolGroupComponent)
};

