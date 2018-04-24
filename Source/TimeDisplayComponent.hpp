#pragma once

#include "TextGlyphComponent.h"
#include "View.hpp"
#include "TimeDisplayController.hpp"

class TimeDisplayComponent : public virtual TextGlphComponent,
							 public virtual View<SymbolistModel, TimeDisplayController > {
	
public:
    TimeDisplayComponent()
    {
        updateText( m_time_str );
    }
    
    inline ~TimeDisplayComponent() {}
    
    inline void setTime( float t )
    {
        m_time = t;
        m_time_str = "t = " + (String)getSymbolistHandler()->getCurrentTime();
        updateText( m_time_str );
    }
    
    inline void toggleView()
    {
        display = !display;
        setVisible(display);
        updateText( m_time_str );
    }
    
    inline bool hitTest (int x, int y) override { return false; }
	
	/* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}
	
private:
	String m_time_str = "t = 0";
    float  m_time     = 0;
    bool   display    = false;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimeDisplayComponent)
    
};
