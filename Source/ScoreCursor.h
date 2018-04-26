#pragma once

#include "SymbolistComponent.h"
#include "StaffComponent.hpp"

using namespace std;

class ScoreCursor : public SymbolistComponent
{
public:
    
    ScoreCursor(){}
    ~ScoreCursor() = default;
    
    void paint( Graphics& g ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void resized() override;
    
    void toggleDisplayState();
    
    void setPlayPoint( float t );
    inline float getPlayPoint(){ return m_playpoint; }
    
private:
    float           m_playpoint = -1;
    bool            display = false;
    
    Point<float>    m_down;
    
    float           minimum_size = 50;
    
    Point<int>      m_prev_dragpoint;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreCursor)
};
