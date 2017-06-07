//
//  PageComponent.hpp
//  symbolist
//
//  Created by Jean Bresson on 04/06/2017.
//
//

#ifndef PageComponent_hpp
#define PageComponent_hpp

#include "PrimitiveIncludes.h"
#include "ScoreComponent.h"

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




class PageComponent : public ScoreComponent, public LassoSource<BaseComponent *>
{
public:
    
    PageComponent();
    ~PageComponent();
    
    // Redefine this from SymbolistComponent
    inline SymbolistComponent* getPageComponent() override { return this; };
    
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
    
    bool                                draw_mode = false;
    LassoComponent< BaseComponent * >   lassoSelector;
    ScoreSelectedItemSet                selected_items;
        
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PageComponent)
};


#endif /* PageComponent_hpp */
