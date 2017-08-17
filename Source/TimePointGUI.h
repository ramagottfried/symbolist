
#pragma once

#include "SymbolistComponent.h"
#include "TimePointArray.h"
#include "SymbolistHandler.h"


/*
 * note: time point display should be aligned with staff it refers to (i.e. the default score shouldn't be "in time")
 * for the time point diplay it might be nice to view it in the staff it refers to? or maybe as an overlay on top of the staff...
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
    
    void resized() override
    {
        auto parent = getParentComponent();
        setBounds(0, parent->getBottom() - 50,  parent->getWidth(), 50 );
    }
    
private:
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimePointGUI)
};
