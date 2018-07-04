#pragma once

#include "TextGlyphComponent.h"
#include "View.hpp"


class TimeDisplayComponent : public virtual TextGlphComponent
{
	
public:
    TimeDisplayComponent()
    {
        updateText( m_time_str );
    }
    
    inline ~TimeDisplayComponent() {}
    
    void setTime( float t );
    
    inline void toggleView()
    {
        display = !display;
        setVisible(display);
        updateText( m_time_str );
    }
    
    inline bool hitTest (int x, int y) override { return false; }
	
private:
	String m_time_str = "t = 0";
    float  m_time     = 0;
    bool   display    = false;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimeDisplayComponent)
    
};
