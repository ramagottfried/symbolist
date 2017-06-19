#ifndef ScoreComponent_h
#define ScoreComponent_h

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistComponent.h"

class BaseComponent ;


class SymbolistLasso : public Component
{
    void paint ( Graphics &g) override;
};


/*
 * Superclass for score-editable containers : PageComponent or SymbolGroupComponent
 * sharing a number of user interactions wrt. editing contents
 */
class ScoreComponent : public SymbolistComponent //, public LassoSource<BaseComponent *>
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
    
    void addToSelection(BaseComponent* c);
    void removeFromSelection(BaseComponent* c);
    void selectAllComponents();
    void unselectAllComponents();
    
    void deleteSelectedSymbols();
    void groupSelectedSymbols();
    void translateSelectedSymbols( Point<int> delta_xy );
    void flipSelectedSymbols( int axis );
    
    void mouseDown ( const MouseEvent& event ) override;
    void mouseMove ( const MouseEvent& event ) override;
    void mouseDrag ( const MouseEvent& event ) override;
    void mouseUp ( const MouseEvent& event ) override;
    void resized () override;
    
    inline void stealMouse(){ component_grabbing_mouse = true; }
    inline void giveBackMouse(){ component_grabbing_mouse = false; }
    
    void notifyEditModeChanged( UI_EditType current_mode );
    
    
protected:

    std::vector<BaseComponent*>     subcomponents;
    
    Array<BaseComponent*>     selected_components;
    SymbolistLasso lasso;
    
    void beginLasso(Point<float> position);
    void dragLasso(Point<float> position);
    void endLasso();

    
    bool                            component_grabbing_mouse = false;

};


#endif
