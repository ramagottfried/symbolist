#ifndef ScoreComponent_h
#define ScoreComponent_h

#include "../JuceLibraryCode/JuceHeader.h"
#include "PrimitiveIncludes.h"
#include "SymbolistComponent.h"

class ScoreSelectedItemSet : public SelectedItemSet<BaseComponent *>
{
public:
    ScoreSelectedItemSet(){};
    ~ScoreSelectedItemSet(){};
    
    virtual void itemSelected (BaseComponent *c) override
    {
//        printf("sel %p \n", c);
        c->select();
    }
    
    virtual void itemDeselected (BaseComponent *c) override
    {
//        printf("desel %p \n", c);
        c->deselect();
    }
    
private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreSelectedItemSet)

};


class ScoreComponent : public SymbolistComponent, public LassoSource<BaseComponent *>
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
    
    
    void translateSelected( Point<int> delta_xy )
    {
        for( auto c : selected_items )
        {
            auto b = c->getBounds();
            c->setTopLeftPosition( b.getPosition() + delta_xy );
        }
    }
    
    // Redefine this from SymbolistComponent
    inline SymbolistComponent* getScoreComponent() override { return this; };
    
    
    // call this when the score has been modified
    // in order to notify and update the host environment
    
    void addScoreChildComponent( BaseComponent* c );
    
    inline Point<float> getScoreMouseDown(){ return m_down; }
//    void setEditMode( UI_EditType m );
    
    // selection
    void findLassoItemsInArea (Array < BaseComponent *>& results, const Rectangle<int>& area) override;
    SelectedItemSet< BaseComponent *>& getLassoSelection() override;
    
    void groupSymbols();
    void deleteSelectedSymbolComponents();
    void removeAllSymbolComponents();
    
    BaseComponent *getNthSymbolComponent (int n ) { return score_stack[n]; }
    
    void scoreSymbolAdded ( BaseComponent* c );
    void scoreSymbolRemoved ( BaseComponent* c );
    void scoreSymbolModified ( BaseComponent* c );
    
    
private:
    bool                                draw_mode = false;
    
    Point<float>                        m_down;
    
    std::vector< BaseComponent * >      score_stack;
    
    LassoComponent< BaseComponent * >   lassoSelector;
    ScoreSelectedItemSet                selected_items;
    
        
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreComponent)
};

#endif