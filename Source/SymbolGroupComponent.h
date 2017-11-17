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
    
    bool hitTest (int x, int y) override;
    
    void h_flip(float ax, float ay) override;
    void v_flip(float ax, float ay) override;

    virtual void rotateScoreComponent(float theta, float ax, float ay) override;
    virtual void scaleScoreComponent(float scale_w, float scale_h) override;


private :
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolGroupComponent)
};

