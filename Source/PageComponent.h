#pragma once

#include "ScoreComponent.h"
#include "BaseComponent.h"
#include "StaffComponent.hpp"

#include "ScoreCursor.h"

/**
 * Represents the page where all the graphic symbols
 * of the score are drawn.
 * PageComponent is a graphic component.
 */
class PageComponent : public virtual ScoreComponent
{

    BaseComponent* edited_component;
    ScoreCursor    score_cursor;

public:
    PageComponent();
    ~PageComponent();
    
    /**
     * Enumerates the different display modes used by the PageComponent class.
     */
    enum DisplayMode { MAIN, EDIT, STAFF };
    
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
        score_cursor.setPlayPoint( t );
    }
    
    void toggleCursorDisplay()
    {
        score_cursor.toggleDisplayState();
    }
    
    void updateTimeCursor()
    {
        score_cursor.repaint();
    }
    
    inline StaffComponent *getStaffByName( String& name )
    {
        
        cout << "num child comps " << getNumChildComponents() << endl;
        return nullptr;
        
        auto c = findChildWithID(name);
        return dynamic_cast<StaffComponent*>(c); // hopefully null if not a StaffComponent
        
    }
    
    vector<BaseComponent*> getSubcomponentsByStaff( String& staff_name );

    DisplayMode getDisplayMode(){ return display_mode; }
    
private:
    DisplayMode         display_mode = MAIN;

    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PageComponent)
};

