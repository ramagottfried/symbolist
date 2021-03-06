#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "Symbol.h"
#include "SymbolistComponent.h"
#include "ScoreComponent.h"
#include <typeinfo>

class BaseComponent : public ScoreComponent {
	
public:
    BaseComponent() = default;
    ~BaseComponent();
    
    virtual void addSymbolMessages( Symbol* s );
    virtual void importFromSymbol( const Symbol &s );
    
    Symbol exportSymbol();

    void parentHierarchyChanged() override;
    void setIdFromSymbol();
	
    void paint( Graphics& g ) override;
	
    // it shouldn't be possible to set the symbol without updating the whole component
    void setScoreSymbol(Symbol* s) { score_symbol = s; }
    Symbol* getScoreSymbol () { return score_symbol; }
    void createAndAttachSymbol();
	
    bool isTopLevelComponent();
    void reportModification();

    const Colour getCurrentColor();
    
    void moved () override;
    void resized () override;

    // these are standard interactions
    void mouseEnter( const MouseEvent& event ) override {};
    void mouseExit( const MouseEvent& event ) override {};
    void mouseMove( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseUp( const MouseEvent& event ) override;
    void mouseDoubleClick( const MouseEvent& event ) override;
    
    virtual void altDragCopy( const MouseEvent& event  );
    
    void recursiveMaximizeBounds();
    void recursiveShrinkBounds();
    
    bool respondsToMouseEvents();
    
    virtual Rectangle<float> symbolExportBounds() { return getBounds().toFloat(); }

    // not very happy with term "Symbol" here
    virtual inline void setSymbolComponentStrokeWeight( float s ){ stroke_weight = s; }
    
    inline void setBoundsFloatRect( Rectangle<float> r )
    {
        setBounds ( r.getX(), r.getY(), r.getWidth(), r.getHeight() );
    }
    
    // set graphical component's bounds from the symbol attributes
    // (can be different for special types of components)
    virtual void setBoundsFromSymbol( float x, float y , float w , float h);
    
    // returns the symbol position from grapical component position and size
    // (can be different for special types of components)
    virtual Point<float> computeSymbolPosition( float x, float y, float w, float h );
    
    virtual void setMinimalBounds ();
    Rectangle<int> getMinimalBounds();

    virtual void setMaximalBounds ();
    
    virtual void resizeToFit(int x, int y, int w, int h);
    virtual void scaleScoreComponent(float scale_w, float scale_h) override;
    virtual void setScoreComponentSize(int w, int h) override;

    void selectComponent() override;
    void deselectComponent() override;
    
    virtual void setEditMode(bool val) override;
    virtual bool isInEditMode() override;
    bool isSelected(){ return is_selected; }
    
    
    // this is all stuff for dealing with groups, maybe should be moved to SymbolGroupComponent?
    /*
    void updateRelativePos();
    void updateRelativeSize();
    virtual void updateRelativeAttributes() override;
     bool inPlaceForRelativeUpdates();
     void addSubcomponent( SymbolistComponent *c ) override;
    //void updateSubcomponents();
     inline void setRelativeBounds( Rectangle<float> rect )
     {
     relative_x = rect.getX();
     relative_y = rect.getY();
     relative_w = rect.getWidth();
     relative_h = rect.getHeight();
     }
     
     inline Rectangle<float> getRelativeBounds()
     {
     return Rectangle<float>( relative_x, relative_y, relative_w, relative_h );
     }
     
*/
    
    inline void setStaffSelectionMode( bool state ) { in_staff_selection_mode = state; }
    void setStaff(StaffComponent* staffComponent);
    inline StaffComponent* getStaff() { return staff; }
    void attachToStaff();
	
protected:
	
    /**
     * Pointer to the score symbol.
     * (set when this is a topLevel symbol, NULL otherwise)
     */
    Symbol* score_symbol = NULL;
	
    string  name;
    string  staff_id;
    string  lambda;
	
    /**
     * Pointer to the graphic component which acts as a staff
     * for this BaseComponent.
     */
    StaffComponent* staff = nullptr; // place holder ...
	
    // when loaded, if staff exists attach it
    // when staff is loaded, scan score and try to find symbols with matching staff names and attach them
	
    // parameters
    float           stroke_weight = 2;
	
    // interaction
    Point<float>    m_down;
    Colour          current_color = Colours::black;
	
	// values between 0.0 and 1.0 relative to the size of its container
    float           relative_x = 0.0, relative_y = 0.0 , relative_w = 1.0 , relative_h = 1.0;
	
	// 0 = scale symbol to bounds, 1 = scale spacing (not resizing)
    int             resize_mode = 0;
    float           m_min_size = 5;
	
    bool            show_bounding_box = false;
    float           bb_strokeWeight = 1;
    Colour          bb_color = Colours::cornflowerblue;
    Colour          sel_color = Colours::cornflowerblue;
	
    bool            in_edit_mode = false;
    bool            in_staff_selection_mode = false;
	
    bool            is_alt_copying = false;
    Point<float>    m_prev_event;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BaseComponent)
    
};


