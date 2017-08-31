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
    
    void enterStaffSelMode();
    void exitStaffSelMode();
    
    // Juce Callbacks
    void resized () override;
    void paint (Graphics& g) override;
    
    void setTimePoint( float t )
    {
        /*
        auto staff = getSymbolistHandler()->getStaveAtTime(t);
        if( staff )
        {
            Symbol* sym = staff->getScoreSymbolPointer();
            
            float play_t = t - sym->getTime();
            float play_x = sym->timeToPixels( play_t );
            score_cursor.setBounds( play_x, staff->getY() + (staff->getHeight() * 0.5), 5, staff->getHeight()+5 );
        }
        */
        score_cursor.setPlayPoint( t );
    }
    
    void toggleCursorDisplay()
    {
        score_cursor.toggleDisplayState();
    }
    
    inline StaffComponent *getStave( String& name )
    {
        
        cout << "num child comps " << getNumChildComponents() << endl;
        return nullptr;
        
        auto c = findChildWithID(name);
        return dynamic_cast<StaffComponent*>(c); // hopefully null if not a StaffComponent
        
    }
    
    vector<BaseComponent*> getSubcomponentsByStaff( String& staff_name );


private:
    
    BaseComponent*      edited_component;
    
    ScoreCursor         score_cursor;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PageComponent)
};

