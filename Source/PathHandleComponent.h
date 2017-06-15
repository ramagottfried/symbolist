
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

class PathHandle : public Component
{
public:
    
    enum handleType {
        anchor,
        curve_control,
        rotate
    };
    
    PathHandle( handleType type, float x, float y, Component *pc);
    ~PathHandle(){}
    
    void paint ( Graphics& g ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    
private:
    
    Point<float>        m_down;
    
    Component           *m_path;
    float               m_size = 10;
    float               m_strokeweight = 1;
    
    handleType          h_type = anchor;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathHandle)
    
};

