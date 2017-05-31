#pragma once

#include "BaseComponent.h"

class CircleComponent : public BaseComponent
{
public:
    CircleComponent();
    CircleComponent( Point<float> pt );
    CircleComponent( float center_x, float center_y, float w = 10, float h = 10, float stroke = 2, Colour color = Colours::black );
    
    ~CircleComponent()
    {
        printf("freeing circle %p\n", this);
    }
    
    void symbol_paint ( Graphics& g ) override;
    
    float symbol_getX() override { return getX() + ( getWidth() * .5) ; }
    float symbol_getY() override { return getY() + ( getHeight() * .5) ; }

    
    void symbol_mouseEnter( const MouseEvent& event ) override {}
    void symbol_mouseMove( const MouseEvent& event ) override
    {
    }
    
    
    void symbol_mouseDown( const MouseEvent& event ) override {}
    void symbol_mouseDrag( const MouseEvent& event ) override
    {
    }
    
    void symbol_mouseExit( const MouseEvent& event ) override {}
    void symbol_mouseDoubleClick( const MouseEvent& event ) override {}
    
    void symbol_mouseEdit( const MouseEvent& event )
    {
        
    }
    
    
private:
    // no local variables here
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CircleComponent)
};
