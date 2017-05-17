#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "PrimitiveIncludes.h"

class ScoreComponent : public Component, public LassoSource<BaseComponent *>
{
public:
    ScoreComponent();
    ~ScoreComponent();
    
    void paint (Graphics& g) override;
    void resized () override;
    
    void mouseMove ( const MouseEvent& event ) override;
    void mouseDown ( const MouseEvent& event ) override;
    void mouseDrag ( const MouseEvent& event ) override;
    void mouseUp ( const MouseEvent& event ) override;
    
    void addScoreChildComponent( BaseComponent *c );
    
    inline Point<float> getScoreMouseDown(){ return m_down; }
    
    void findLassoItemsInArea (Array < BaseComponent *>& results, const Rectangle<int>& area) override;
    SelectedItemSet< BaseComponent *>& getLassoSelection() override;
    
private:
    Point<float> m_down;
    
    std::vector<BaseComponent *> score_stack;
    
    LassoComponent< BaseComponent * > lassoSelector;
    SelectedItemSet<BaseComponent *> selected_items;
    
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreComponent)
};
