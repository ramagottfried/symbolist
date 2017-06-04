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
    
    virtual void itemSelected (BaseComponent *c) override { c->selectComponent(); }
    virtual void itemDeselected (BaseComponent *c) override { c->deselectComponent(); }
    
private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreSelectedItemSet)
};



class ScoreComponent : public SymbolistComponent, public LassoSource<BaseComponent *>
{
public:
    
    ScoreComponent();
    ~ScoreComponent();
    
    // Redefine this from SymbolistComponent
    inline SymbolistComponent* getScoreComponent() override { return this; };
    
    BaseComponent *getNthSymbolComponent (int n ) { return score_stack[n]; }
    

    void addChildToScoreComponent( BaseComponent* c );
    void removeChildFromScoreComponent( BaseComponent* c );
    void clearAllSymbolComponents();


    void addItemToSelection(BaseComponent *c);
    void deselectAllSelected();
    void deleteSelectedSymbols();
    void addSymbolAt ( Point<float> p );
    
    // call this when the score has been modified
    // in order to notify and update the host environment
    void addSymbolToScore ( BaseComponent* c );
    void removeSymbolFromScore ( BaseComponent* c );
    void modifySymbolInScore ( BaseComponent* c );
    
    
    void groupSymbols();

    // selection
    void findLassoItemsInArea (Array < BaseComponent *>& results, const Rectangle<int>& area) override;
    SelectedItemSet< BaseComponent *>& getLassoSelection() override;
    void translateSelected( Point<int> delta_xy );
    
    // Juce Calbacks
    void paint (Graphics& g) override;
    void resized () override;
    
    void mouseMove ( const MouseEvent& event ) override;
    void mouseDown ( const MouseEvent& event ) override;
    void mouseDrag ( const MouseEvent& event ) override;
    void mouseUp ( const MouseEvent& event ) override;

    
private:
    
    
    std::vector< BaseComponent * >      score_stack;
    
    bool                                draw_mode = false;
    LassoComponent< BaseComponent * >   lassoSelector;
    ScoreSelectedItemSet                selected_items;
    std::vector< BaseComponent * >      selected_items_2;
    
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreComponent)
};

#endif
