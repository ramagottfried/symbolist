
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "ScoreData.h"

class BaseComponent : public Component
{
public:
    BaseComponent();
    ~BaseComponent();
    
    void paint ( Graphics& g ) override;
    void moved () override;
    void resized () override;
    
    void mouseEnter( const MouseEvent& event ) override;
    void mouseMove( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseExit( const MouseEvent& event ) override;
    void mouseDoubleClick( const MouseEvent& event ) override;

    
    // subroutine in derived class, maybe return bool to trigger repaint
    virtual void symbol_paint ( Graphics& g ){}
    virtual void symbol_moved (){}
    virtual void symbol_resized (){}
    
    virtual void symbol_mouseEnter( const MouseEvent& event ){}
    virtual void symbol_mouseMove( const MouseEvent& event ){}
    virtual void symbol_mouseDown( const MouseEvent& event ){}
    virtual void symbol_mouseDrag( const MouseEvent& event ){}
    virtual void symbol_mouseExit( const MouseEvent& event ){}
    virtual void symbol_mouseDoubleClick( const MouseEvent& event ){}


    
    inline void attachScore(Component *s){ the_score = s; };
    inline Component *getScore(){ return the_score; };
    inline void setSymbol(Symbol *s){ score_symbol = s; };
    
    
    // add osc score w/r here?
    
protected:
    // parameters
    Rectangle<int>  bounds;
    float           strokeWeight = 1;
    Colour          sym_color = Colours::black;
    
    // interaction
    Point<float>    m_down;
    Colour          current_color = Colours::black;

    
    bool            is_selected = false;
    bool            showBoundingBox = false;
    float           bb_strokeWeight = 1;
    Colour          bb_color = Colours::cornflowerblue;
    Colour          sel_color = Colours::cornflowerblue;
    
    /*
     
     to do: setup selection system:
        if selected and in the score context, then create handles for resizing (if it makes sense for the mode)
        if selected and in the palette context, highlight and set type for drawing
     
     */
    
private:
    Component *the_score;
    Symbol *score_symbol;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BaseComponent)
    
};



// all parameters that might be used for performance should be stored in the score,
// separate from the graphic component class, if possible, generalize the stored parameter namespace here so that
// all inherited children can read and write their states through this interface
