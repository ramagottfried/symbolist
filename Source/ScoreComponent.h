#ifndef ScoreComponent_h
#define ScoreComponent_h

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistComponent.h"

class BaseComponent ;


class SymbolistLasso : public Component
{
    
public:
    
    void paint ( Graphics &g) override;
    void begin(int x, int y);
    void update(int x, int y);
    void end();
    
private:
    
    float start_x, start_y;
};


/*********************
 * Superclass for score-editable containers : PageComponent or BaseComponent (when in_edit_mode)
 *********************/
class ScoreComponent : public SymbolistComponent
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
    
    virtual void mouseAddClick ( Point<float> p );
    
    void addToSelection(BaseComponent* c);
    void removeFromSelection(BaseComponent* c);
    void selectAllComponents();
    void unselectAllComponents();
    
    void deleteSelectedSymbols();
    void groupSelectedSymbols();
    void ungroupSelectedSymbols();
    void translateSelectedSymbols( Point<int> delta_xy );
    void flipSelectedSymbols( int axis );
    
    void mouseDown ( const MouseEvent& event ) override;
    void mouseDrag ( const MouseEvent& event ) override;
    void mouseUp ( const MouseEvent& event ) override;

protected:

    std::vector<BaseComponent*>     subcomponents;
    
    Array<BaseComponent*>     selected_components;
    SymbolistLasso s_lasso;
    
    void beginLassoSelection(Point<int> position);
    void dragLassoSelection(Point<int> position);
    void endLassoSelection();

};


#endif
