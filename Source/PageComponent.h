#pragma once

#include "BaseComponent.h"
#include "StaffComponent.hpp"
#include "ScoreCursor.h"
#include "View.hpp"
#include "SymbolistModel.hpp"
#include "PageController.hpp"

/**
 * Represents the page where all the graphic symbols
 * of the score are drawn.
 * PageComponent is a graphic component.
 */
class PageComponent : public virtual ScoreComponent,
                      public virtual View<SymbolistModel, PageController> {

public:
    PageComponent();
    ~PageComponent();
    
    /**
     * Enumerates the different display modes used by the PageComponent class.
     */
    enum DisplayMode { MAIN, EDIT, STAFF };
    
    // Inherited from SymbolistComponent.
    inline PageComponent* getPageComponent() override { return this; };
    
    // Redefine from ScoreComponents for special actions (update the score)
    void addSubcomponent(SymbolistComponent *c) override ;
    void removeSubcomponent(SymbolistComponent *c) override ;
    
    virtual void groupSelectedSymbols() override;
    
    /**
	 * Wrapper method around the PageController::copySelectedToClipBoard()
	 * method.
	 *
	 * @see PageController#copySelectedToClipBoard()
	 *      PageController::copySelectedToClipBoard
	 */
    void copySelectedToClipBoard();
    
    /**
	 * Wrapper method around the PageController::newFromClipBoard()
	 * method.
	 *
	 * @see PageController#newFromClipBoard() PageController::newFromClipBoard
	 */
    void newFromClipBoard();
    
    // single_component edit mode
    void enterEditMode( BaseComponent* c );
    void exitEditMode();
    ScoreComponent* getEditedComponent();
    
    void enterStaffSelMode();
    void exitStaffSelMode();
    
    /**
     * Creates a staff symbol in the score and a staff component
     * in the score view from the selected components.
     *
     * This method is only accessible in the PageComponent class,
     * meaning that staves cannot be created within staff, but
     * only at a top level in the score.
     */
    void createStaffFromSelected();
    
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
    
    /* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}
    
private:
    DisplayMode    display_mode = MAIN;
	BaseComponent* edited_component;
    ScoreCursor    score_cursor;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PageComponent)
};

