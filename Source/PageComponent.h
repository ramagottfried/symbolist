//
//  PageComponent.hpp
//  symbolist
//
//  Created by Jean Bresson on 04/06/2017.
//
//

#ifndef PageComponent_hpp
#define PageComponent_hpp

#include "ScoreComponent.h"
#include "BaseComponent.h"
#include "ScoreCursor.h"
#include "TimePointGUI.h"

class PageComponent : public ScoreComponent
{
public:
    
    PageComponent();
    ~PageComponent();
    
    // Redefine this from SymbolistComponent
    inline PageComponent* getPageComponent() override { return this; };
    
    // redefine from ScoreComponents for special actions (update the score)
    void    addSubcomponent ( SymbolistComponent *c ) override ;
    void    removeSubcomponent( SymbolistComponent *c ) override ;
    
    // single_component edit mode
    void enterEditMode( BaseComponent* c );
    void exitEditMode();
    ScoreComponent* getEditedComponent();
    
    // Juce Callbacks
    void resized () override;
    void paint (Graphics& g) override;
    
    void setTimePoint( float t )
    {
        score_cursor.setPlayPoint( t );
    }
    
private:
    
    BaseComponent*      edited_component;
    
    ScoreCursor         score_cursor;
    TimePointGUI        time_pointGUI;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PageComponent)
};


#endif /* PageComponent_hpp */
