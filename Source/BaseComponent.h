#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "Symbol.h"
#include "SymbolistComponent.h"
#include "ScoreComponent.h"

template <typename T>
void printRect( const Rectangle<T> &rect, const String &name = "rect" )
{
    std::cout << name << " " << rect.getX() << " " << rect.getY() << " " << rect.getWidth() << " " << rect.getHeight() << std::endl ;
}

template <typename T>
void printPoint(Point<T> point, const String &name = "point" )
{
    std::cout << name << " " << point.getX() << " " << point.getY() << "\n";
}


class BaseComponent : public ScoreComponent
{
public:
    
    BaseComponent() = default;
    ~BaseComponent() = default;

    virtual int addSymbolMessages( Symbol* s, const String &base_address );
    virtual void importFromSymbol( const Symbol &s );
    
    void paint ( Graphics& g ) override;
    
    void setScoreSymbolPointer (Symbol* s) { score_symbol = s; }
    Symbol* getScoreSymbolPointer () { return score_symbol; }
    void createAndAttachSymbol();
    
    bool isTopLevelComponent();
    void reportModification();

    const Colour getCurrentColor();
    
    void moved () override;
    void resized () override;

    virtual void h_flip() {}
    virtual void v_flip() {}
    
    // these are standard interactions
    void mouseEnter( const MouseEvent& event ) override {};
    void mouseExit( const MouseEvent& event ) override {};
    void mouseMove( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseUp( const MouseEvent& event ) override;
    void mouseDoubleClick( const MouseEvent& event ) override;
    
    void altDragCopy( const MouseEvent& event  );
    
    void recursiveMaximizeBounds();
    void recursiveShrinkBounds();
    
    bool respondsToMouseEvents();
    
    virtual Rectangle<float> symbol_export_bounds(){ return getBounds().toFloat(); }
    const String& symbol_export_name()
    {
        cout << "name " << getComponentID() <<  " " << getSymbolTypeStr() << endl;
        return ( !name.isEmpty() ? name : getComponentID() );
    }
    
    // not very happy with therm "Symbol" here
    inline void setSymbolStrokeWeight( float s ){ strokeWeight = s; }
    inline void setSymbolColor( Colour c ){ sym_color = c; }
    
    
    // helper functions
    inline void symbol_debug_function(const char* func)
    {
        std::cout << juce::Time::currentTimeMillis() << " " << getSymbolTypeStr() << " " << this << " " << func << std::endl;
    }
    
    
    inline void setBoundsFloatRect( Rectangle<float> r )
    {
        setBounds ( r.getX(), r.getY(), r.getWidth(), r.getHeight() );
    }

    virtual void setBoundsFromSymbol( float x, float y , float w , float h);
        
    virtual void setMinimalBounds ();
    virtual void setMaximalBounds ();
    
    virtual void resizeToFit(int x, int y, int w, int h);
    
    void selectComponent() override;
    void deselectComponent() override;
    
    virtual void setEditMode(bool val);
    bool isInEditMode();
    bool isSelected(){ return is_selected; }
    
    void updateRelativePos();
    void updateRelativeSize();
    void updateRelativeAttributes();
    void updateSubcomponents();
    void addSubcomponent( SymbolistComponent *c ) override;
    bool inPlaceForRelativeUpdates();
    
protected:
    
    // score structure
    Symbol*         score_symbol;   // poiner to the score symbol (set when this is a topLevel symbol, NULL otherwise)
    
    String          name;
    String          staff_name;
    
    // parameters
    float           strokeWeight = 2;
    Colour          sym_color = Colours::black;
    
    // interaction
    Point<float>    m_down;
    Colour          current_color = Colours::black;

    ScopedPointer<ResizableBorderComponent> resizableBorder;
    ComponentBoundsConstrainer              constrainer;
    
    float           relative_x = 0.0, relative_y = 0.0 , relative_w = 1.0 , relative_h = 1.0 ;  // values between 0.0 and 1.0 relative to the size of its container
    int             resize_mode = 0; // 0 = scale symbol to bounds, 1 = scale spacing (not resizing)
    float           m_min_size = 5;
   
    bool            showBoundingBox = false;
    float           bb_strokeWeight = 1;
    Colour          bb_color = Colours::cornflowerblue;
    Colour          sel_color = Colours::cornflowerblue;
    
    bool            in_edit_mode = false;

    bool            is_alt_copying = false;
    Point<float>    m_prev_event;

private:
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BaseComponent)
    
};

