
#pragma once

#include "SymbolistComponent.h"
#include "TimePointArray.h"
#include "SymbolistHandler.h"


/*
 * note: time point display should be aligned with staff it refers to (i.e. the default score shouldn't be "in time")
 * for the time point diplay it might be nice to view it in the staff it refers to? or maybe as an overlay on top of the staff...
 * also note that the time points should scale with the score zoom, which means that possibly they *should* be children of the page component --- or even, children of the Staff component
 *
 */

class TimePointGUI : public SymbolistComponent
{
public:
    TimePointGUI(){}
    ~TimePointGUI(){}
    
    void paint( Graphics& g ) override
    {
        auto timepoints = getSymbolistHandler()->getTimePointArray();
        
        for( auto t : (*timepoints) )
        {
            g.fillEllipse( t->time * 100.0f, getHeight() / 2, 2, 2);
        }
    }

    bool hitTest (int x, int y) override { return false; }

private:
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimePointGUI)
};
