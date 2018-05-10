
#pragma once

#include "JuceHeader.h"
#include "ScoreComponent.h"

class PathHandle : public ScoreComponent {

public:
    
    enum handleType {
        start,
        anchor,
        quadratic_control,
        cubic_control,
        rotate
    };
    
    PathHandle( handleType type, float x, float y);
    ~PathHandle(){}
    
    string getSymbolTypeStr() const override { return string("path_point"); }
    
    void paint ( Graphics& g ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseDoubleClick(const MouseEvent& event) override;

    handleType getHandleType() { return h_type; }
    void setHandleType(handleType type) { h_type = type ; }
    bool isClosing() { return closing; }
    void setClosing(bool val) { closing = val; }
    bool isEnd() { return end; }
    void setEnd(bool val) { end = val; }
    
    Point<float> getCenter() const;
    
private:
    
    Point<float>        m_down;
    
    float               m_size = 10;
    float               m_strokeweight = 1;
    
    handleType          h_type = anchor;
    bool                closing = false;
    bool                end = false;
    
    float               m_prev_theta;
    
    Rectangle<float>    m_anchor_bounds;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathHandle)
    
};

