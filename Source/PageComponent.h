#pragma once

#include "ScoreComponent.h"
#include "BaseComponent.h"
#include "StaffComponent.hpp"

#include "ScoreCursor.h"

class PageComponent : public ScoreComponent
{
public:
    
    PageComponent();
    ~PageComponent();
    
    // Redefine this from SymbolistComponent
    inline PageComponent* getPageComponent() override { return this; };
    
    // redefine from ScoreComponents for special actions (update the score)
    void addSubcomponent ( SymbolistComponent *c ) override ;
    void removeSubcomponent( SymbolistComponent *c ) override ;
    
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
    
        StaffComponent *getStave( String& name )
    {
        auto c = findChildWithID(name);
        return dynamic_cast<StaffComponent*>(c); // hopefully null if not a StaffComponent
        
    }
    
private:
    
    BaseComponent*      edited_component;
    
    ScoreCursor         score_cursor;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PageComponent)
};

