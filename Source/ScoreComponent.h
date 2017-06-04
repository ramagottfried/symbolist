#ifndef ScoreComponent_h
#define ScoreComponent_h

#include "../JuceLibraryCode/JuceHeader.h"

#include "SymbolistComponent.h"
#include "BaseComponent.h"

/*
 * Superclass for score-editable containers : PageComponent or SymbolGroupComponent
 * sharing a number of user interactions wrt. editing contents
 */
class ScoreComponent : public SymbolistComponent
{
public:
    
    ScoreComponent() = default;
    ~ScoreComponent() = default;
    
    void addChildToScoreComponent( BaseComponent* c );
    void removeChildFromScoreComponent( BaseComponent* c , bool delete_it);
    void clearAllSymbolComponents();
    
    
    //void deleteSelectedSymbols();
    //void addSymbolAt ( Point<float> p );

protected:

    std::vector<BaseComponent*>     subcomponents;
    
};


#endif
