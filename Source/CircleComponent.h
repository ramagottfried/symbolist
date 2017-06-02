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
    
    
private:
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CircleComponent)
};
