#pragma once

#include "BaseComponent.h"


class PathHandle : public BaseComponent
{
public:
    PathHandle( Point<float> startPT )
    {
        setBounds( startPT.getX(), startPT.getY(), 10, 10);
    }
    
    ~PathHandle();
    
    void symbol_paint ( Graphics& g ) override
    {
        g.setColour ( current_color );
        const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( strokeWeight );
        g.drawRect ( bounds, (float) strokeWeight );
    }
    
    void symbol_moved () override;
    void symbol_resized () override;
    
    void symbol_mouseEnter( const MouseEvent& event ) override;
    void symbol_mouseMove( const MouseEvent& event ) override;
    void symbol_mouseDown( const MouseEvent& event ) override;
    void symbol_mouseDrag( const MouseEvent& event ) override;
    void symbol_mouseUp( const MouseEvent& event ) override;
    
    void symbol_mouseExit( const MouseEvent& event ) override;
    void symbol_mouseDoubleClick( const MouseEvent& event ) override;
    
private:

    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathHandle)

};


class PathComponent : public BaseComponent
{
public:
    PathComponent();
    
    PathComponent( Point<float> startPT );
    ~PathComponent();
    
    void printPath( Path p );
    
    void symbol_paint ( Graphics& g ) override;
    
    void symbol_moved () override;
    void symbol_resized () override;
    
    void symbol_mouseEnter( const MouseEvent& event ) override;
    void symbol_mouseMove( const MouseEvent& event ) override;
    void symbol_mouseDown( const MouseEvent& event ) override;
    void symbol_mouseDrag( const MouseEvent& event ) override;
    void symbol_mouseUp( const MouseEvent& event ) override;
            
    void symbol_mouseExit( const MouseEvent& event ) override;
    void symbol_mouseDoubleClick( const MouseEvent& event ) override;
    
private:
    
    Point<float>    m_drag;
    PathStrokeType  strokeType;
    Path            m_path;
    
    
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathComponent)
};
