#ifndef ScoreComponent_h
#define ScoreComponent_h

#include "../JuceLibraryCode/JuceHeader.h"

#include "SymbolistComponent.h"

/*
 * Superclass for score-editable containers : PageComponent or SymbolGroupComponent
 * sharing a number of user interactions wrt. editing contents
 */
class ScoreComponent : public SymbolistComponent
{
public:
    
    ScoreComponent() = default;
    ~ScoreComponent() = default;
    
    //void deleteSelectedSymbols();
    //void addSymbolAt ( Point<float> p );

private:
    
    
};


#endif
