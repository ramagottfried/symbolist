
#ifndef BaseComponent_h
#define BaseComponent_h

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
    
    BaseComponent(const Symbol &s);
    ~BaseComponent();
    
    virtual String getSymbolTypeStr() const = 0 ; // { return "symbol"; }

    void paint ( Graphics& g ) override;
    
    void setScoreSymbolPointer (Symbol* s) { score_symbol = s; }
    Symbol* getScoreSymbolPointer () { return score_symbol; }
    
    bool isTopLevelComponent();
    void reportModification();
        
    virtual void componentCretated() {}
    
    virtual int addSymbolMessages( Symbol* s, const String &base_address );
    
    virtual void importFromSymbol( const Symbol &s );

    // Called from the Juce::SelectedItemSet subclass in ScoreComponent
    // specific methd defined not to mess with existing select system
    virtual void selectComponent();
    virtual void deselectComponent();
    
    inline const Colour getCurrentColor(){ return is_selected ? sel_color : sym_color; }

    // these four modify the symbol
    void moved () override;
    void resized () override;
    virtual void h_flip(){}
    virtual void v_flip(){}
    
    // these are standard interactions
    void mouseEnter( const MouseEvent& event ) override {};
    void mouseExit( const MouseEvent& event ) override {};
    void mouseMove( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseUp( const MouseEvent& event ) override;
    void mouseDoubleClick( const MouseEvent& event ) override;
        
    void recursiveMaximizeBounds();
    void recursiveShrinkBounds();
    
    bool respondsToMouseEvents();
    
    Point<float> shiftConstrainMouseAngle( const MouseEvent& event );

    
    virtual float symbol_export_X(){ return getX(); }
    virtual float symbol_export_Y(){ return getY(); }
        
    // not very happy with therm "Symbol" here
    inline void setSymbolStrokeWeight( float s ){ strokeWeight = s; }
    inline void setSymbolColor( Colour c ){ sym_color = c; }
    
    virtual void notifyEditModeChanged( UI_EditType current_mode ){}
    
    
    // helper functions
    inline void symbol_debug_function(const char* func)
    {
        std::cout << juce::Time::currentTimeMillis() << " " << getSymbolTypeStr() << " " << this << " " << func << std::endl;
    }
    
    inline bool symbol_parse_error( int p, const String& address )
    {
        if( p == -1 )
        {
            std::cout << "failed to parse symbol:\t" << address << std::endl;
            return true; // there is an error
        }
        return false;
    }
    
    inline void setBoundsFloatRect( Rectangle<float> r )
    {
        setBounds ( r.getX(), r.getY(), r.getWidth(), r.getHeight() );
    }

    virtual void setBoundsFromSymbol( float x, float y , float w , float h);
        
    virtual void setMinimalBounds ();
    virtual void setMaximalBounds ();
    
    void setEditMode(bool val) { in_edit_mode = val; }
    bool isInEditMode() { return in_edit_mode; }
    void setSeleted(bool val) { is_selected = val; }
    bool isSelected() { return is_selected; }


protected:
    
    // score structure
    Symbol*         score_symbol;   // poiner to the score symbol (set when this is a topLevel symbol, NULL otherwise)
    
    // parameters
    float           strokeWeight = 2;
    Colour          sym_color = Colours::black;
    
    // interaction
    Point<float>    m_down;
    Colour          current_color = Colours::black;

    ScopedPointer<ResizableBorderComponent> resizableBorder;
    ComponentBoundsConstrainer              constrainer;
    
    int             resize_mode = 0; // 0 = scale symbol to bounds, 1 = scale spacing (not resizing)
    float           m_min_size = 5;
        
    bool            showBoundingBox = false;
    float           bb_strokeWeight = 1;
    Colour          bb_color = Colours::cornflowerblue;
    Colour          sel_color = Colours::cornflowerblue;
    
    bool            is_selected = false;
    bool            in_edit_mode = false;
        
        
private:
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BaseComponent)
    
};

#endif

// all parameters that might be used for performance should be stored in the score,
// separate from the graphic component class, if possible, generalize the stored parameter namespace here so that
// all inherited children can read and write their states through this interface
