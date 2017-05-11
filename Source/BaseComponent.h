
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"


class BaseComponent : public Component
{
public:
    BaseComponent(){}
    ~BaseComponent(){}
    
    // add osc score w/r here?
    
private:
    void *oscbundle;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BaseComponent)
    
};



// all parameters that might be used for performance should be stored in the score,
// separate from the graphic component class, if possible, generalize the stored parameter namespace here so that
// all inherited children can read and write their states through this interface