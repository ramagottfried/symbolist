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


class PageComponent : public ScoreComponent
{
public:
    
    PageComponent();
    ~PageComponent() = default;
    
    // Redefine this from SymbolistComponent
    inline SymbolistComponent* getPageComponent() override { return this; };
    
    // redefine from ScoreComponents for special actions (update the score)
    void    addSymbolComponent ( BaseComponent *c ) override;
    void    removeSymbolComponent( BaseComponent* c ) override;
    
    // Juce Callbacks
    void paint (Graphics& g) override;
    
private:
    
        
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PageComponent)
};


#endif /* PageComponent_hpp */
