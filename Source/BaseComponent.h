
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "ScoreData.h"

template <typename T>
void printRect( Rectangle<T> rect, String name = "rect" )
{
    std::cout << name << " " << rect.getX() << " " << rect.getY() << " " << rect.getWidth() << " " << rect.getHeight() << "\n";
}

class BaseComponent : public Component
{
public:
    BaseComponent();
    BaseComponent( Point<float> startPt );
    ~BaseComponent();
    
    void select();
    void deselect();
    
    void paint ( Graphics& g ) override;
    void moved () override;
    void resized () override;
    
    void mouseEnter( const MouseEvent& event ) override;
    void mouseMove( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseUp( const MouseEvent& event ) override;

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
    virtual void symbol_mouseUp( const MouseEvent& event ){}
    virtual void symbol_mouseExit( const MouseEvent& event ){}
    virtual void symbol_mouseDoubleClick( const MouseEvent& event ){}

    inline void attachScoreView(Component *c){ score_view = c; };
    inline Component *getScoreView(){ return score_view; };
    inline void setSymbol(Symbol *s){ score_symbol = s; };
    inline Symbol* getSymbol(){ return score_symbol; };
    
    inline void setSymbolStrokeWeight( float s ){ strokeWeight = s; }
    inline void setSymbolColor( Colour c ){ sym_color = c; }
    
    inline void setEditMode( bool e ){ is_being_edited = e; }
    
protected:
    // parameters
    float           strokeWeight = 1;
    Colour          sym_color = Colours::black;
    
    // interaction
    Point<float>    m_down;
    Colour          current_color = Colours::black;

    ScopedPointer<ResizableBorderComponent> resizableBorder;
    
    
    int             resize_mode = 0; // 0 = scale symbol to bounds, 1 = scale spacing (not resizing)
    
    
    bool            showBoundingBox = false;
    float           bb_strokeWeight = 1;
    Colour          bb_color = Colours::cornflowerblue;
    Colour          sel_color = Colours::cornflowerblue;
    
    /*
     
     to do: setup selection system:
        if selected and in the score context, 
        if clicked after selected (? or if it's the only selection) 
            then create handles for resizing (if it makes sense for the mode)
        if selected and in the palette context, highlight and set type for drawing
     
     */

    bool        is_being_edited = true;
    bool        is_selected = false;
    
private:

    Component   *score_view;
    Symbol      *score_symbol;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BaseComponent)
    
};



// all parameters that might be used for performance should be stored in the score,
// separate from the graphic component class, if possible, generalize the stored parameter namespace here so that
// all inherited children can read and write their states through this interface
