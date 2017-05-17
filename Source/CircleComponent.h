#pragma once

#include "BaseComponent.h"

class CircleComponent : public BaseComponent
{
public:
    CircleComponent();
    CircleComponent( float x, float y, float diameter = 10, float stroke = 2, Colour color = Colours::black );
    
    ~CircleComponent();
    
    void symbol_paint ( Graphics& g ) override;
    void symbol_moved () override {}
    void symbol_resized () override {}
    
    void symbol_mouseEnter( const MouseEvent& event ) override {}
    void symbol_mouseMove( const MouseEvent& event ) override {}
    void symbol_mouseDown( const MouseEvent& event ) override {}
    void symbol_mouseDrag( const MouseEvent& event ) override {}
    void symbol_mouseExit( const MouseEvent& event ) override {}
    void symbol_mouseDoubleClick( const MouseEvent& event ) override;
    
    
private:
    // local parameters for this shape:
    //    none

    
    // bounds are determined by binding box, the symbol is abstract and normalized to the size of the box
    /*
     inherited:
        
     bounds (x,y,w,h)
     sel_color
     strokeweight
     
     */
    

    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CircleComponent)
};
