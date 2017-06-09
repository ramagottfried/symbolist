
#ifndef BaseComponent_h
#define BaseComponent_h

#include "../JuceLibraryCode/JuceHeader.h"

#include "Symbol.h"
#include "SymbolistComponent.h"

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

template <typename T>
Rectangle<T> rectangle_stretch(Rectangle<T> r, T w, T h)
{
    return r.withSize( r.getWidth() + w, r.getHeight() + h );
}


class BaseComponent : public SymbolistComponent
{
public:
    
    BaseComponent(const String &type,
                  float center_x, float center_y,
                  float w = 10, float h = 10,
                  float stroke = 2,
                  Colour color = Colours::black  );
    
    ~BaseComponent();
    
    String getSymbolType();
    Symbol* getInternalSymbol();
    
    bool isTopLevelComponent();

    void updateInternalSymbol();
    void addSymbolToScore();
    void removeSymbolFromScore();
    
    virtual int addSymbolMessages( Symbol* s, const String &base_address );
    virtual void importFromSymbol( const Symbol* s );

    
    // Called from the Juce::SelectedItemSet subclass in PageComponent
    // specific methd defined not to mess with existing select system
    virtual void selectComponent();
    virtual void deselectComponent();
    
    // callbacks redefinitions from Juce::Component
    void paint ( Graphics& g ) override;
    // subroutine in derived class, maybe return bool to trigger repaint
    virtual void symbol_paint ( Graphics& g ) {}

    // these two modify the symbol
    void moved () override;
    void resized () override;

    
    // these are standard interactions
    void mouseEnter( const MouseEvent& event ) override {};
    void mouseExit( const MouseEvent& event ) override {};
    void mouseMove( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseUp( const MouseEvent& event ) override;
    void mouseDoubleClick( const MouseEvent& event ) override {};
    
    virtual float symbol_getX(){ return getX(); }
    virtual float symbol_getY(){ return getY(); }
    
    // not very happy with therm "Symbol" here
    inline void setSymbolStrokeWeight( float s ){ strokeWeight = s; }
    inline void setSymbolColor( Colour c ){ sym_color = c; }
    
    inline void setEditState( bool e ){ is_being_edited = e; }
    
protected:
    
    // score structure
    Symbol                          *score_symbol;
    Symbol                          internal_symbol;
    
    String                          symbol_type;

    // parameters
    float           strokeWeight = 2;
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
    
    
    inline void setBoundsFloatRect( Rectangle<float> r )
    {
        setBounds( r.getX(), r.getY(), r.getWidth(), r.getHeight() );
    }
    
   
    inline void symbol_debug_function(const char* func)
    {
        std::cout << juce::Time::currentTimeMillis() << " " << symbol_type << " " << this << " " << func << std::endl;
    }
    
private:
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (BaseComponent)
    
};

#endif

// all parameters that might be used for performance should be stored in the score,
// separate from the graphic component class, if possible, generalize the stored parameter namespace here so that
// all inherited children can read and write their states through this interface
