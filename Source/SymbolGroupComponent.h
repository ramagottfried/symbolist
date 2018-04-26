#pragma once

#include "BaseComponent.h"

class SymbolGroupComponent : public BaseComponent {
    
public:
    
    SymbolGroupComponent() = default;
    virtual ~SymbolGroupComponent() = default;
    
    virtual string getSymbolTypeStr() const override { return string("group"); }
    void addSymbolMessages(Symbol* s ) override ;
    void importFromSymbol( const Symbol &s ) override;

	virtual void groupSelectedSymbols() override;
	void deleteSelectedComponents() override;
	
    void selectComponent() override;
    void deselectComponent() override;
    void setEditMode( bool val ) override;
    void paint ( Graphics& g ) override;
    bool intersectRect( Rectangle<int> rect) override;
    
    virtual bool hitTest (int x, int y) override;
    
    void h_flip(float ax, float ay) override;
    void v_flip(float ax, float ay) override;

    virtual void rotateScoreComponent(float theta, float ax, float ay ) override;
    virtual void scaleScoreComponent(float scale_w, float scale_h) override;
    
    virtual void setSymbolComponentColor( Colour c ) override;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolGroupComponent)
};

