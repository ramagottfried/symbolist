#ifndef ScoreComponent_h
#define ScoreComponent_h

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistComponent.h"

class BaseComponent ;

class ScoreSelectedItemSet : public SelectedItemSet<BaseComponent *>
{
public:
    ScoreSelectedItemSet() = default ;
    ~ScoreSelectedItemSet() = default;
    
    virtual void itemSelected (BaseComponent *c) override;
    virtual void itemDeselected (BaseComponent *c) override;
    
private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreSelectedItemSet)
};



/*
 * Superclass for score-editable containers : PageComponent or SymbolGroupComponent
 * sharing a number of user interactions wrt. editing contents
 */
class ScoreComponent : public SymbolistComponent, public LassoSource<BaseComponent *>
{
public:
    
    ScoreComponent()
    {
        setComponentID("ScoreComponent");
    }
    
    ~ScoreComponent();
    
    const size_t    getNumSubcomponents( );
    BaseComponent*  getSubcomponent( int i );
    void            addSubcomponent( BaseComponent *c );
    void            removeSubcomponent( BaseComponent *c );
    void            clearAllSubcomponents();
    
    virtual void    addSymbolComponent( BaseComponent *c );
    virtual void    removeSymbolComponent( BaseComponent *c );
    
    BaseComponent*  mouseAddSymbolAt ( Point<float> p );
    
    // selection
    void findLassoItemsInArea (Array < BaseComponent *>& results, const Rectangle<int>& area) override;
    SelectedItemSet< BaseComponent *>& getLassoSelection() override { return selected_items; };
    
    void deleteSelectedSymbols();
    void deselectAllSelected();
    void groupSelectedSymbols();

    void translateSelected( Point<int> delta_xy );
    void flipSelected( int axis );
    
    void mouseDown ( const MouseEvent& event ) override;
    void mouseMove ( const MouseEvent& event ) override;
    void mouseDrag ( const MouseEvent& event ) override;
    void mouseUp ( const MouseEvent& event ) override;
    void resized () override;
    
    inline void stealMouse(){ component_grabbing_mouse = true; }
    inline void giveBackMouse(){ component_grabbing_mouse = false; }
    
    void notifyEditModeChanged( UI_EditType current_mode );
    
    void activateLasso();
    void deactivateLasso();
    
protected:

    std::vector<BaseComponent*>     subcomponents;
    
    LassoComponent<BaseComponent*>  lassoSelector;
    ScoreSelectedItemSet            selected_items;
    
    bool                            component_grabbing_mouse = false;

};


#endif
