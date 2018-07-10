#pragma once

#ifndef EditSelectionBox_h
#define EditSelectionBox_h

#include "JuceHeader.h"
#include "PathHandleComponent.h"
#include "Symbol.h"
#include "Zone.hpp"

/**
 * Creates a resizer.
 *
 * Adapted from JUCE ResizableBoarderComponent.
 *
 * Pass in the target component which you want to be resized when this one is
 * dragged.
 *
 * The target component will usually be a parent of the resizer component, but this
 * isn't mandatory.
 *
 * Remember that when the target component is resized, it'll need to move and
 * resize this component to keep it in place, as this won't happen automatically.
 *
 * If the constrainer parameter is not a nullptr, then this object will be used to
 * enforce limits on the size and position that the component can be stretched to.
 * Make sure that the constrainer isn't deleted while still in use by this object.
 *
 * @see ComponentBoundsConstrainer
 */
class EditSelectionBox  : public Component {

public:
	
    EditSelectionBox (Array<ScoreComponent*>* const selected_component_array );
    ~EditSelectionBox();

	/*******************************
	 *       GETTERS & SETTERS     *
	 *******************************/
	inline Array<ScoreComponent* >* getComponentSet() { return component_set; }
	
    void setBorderThickness (const BorderSize<int>& newborder_size);
    BorderSize<int> getBorderThickness() const;
    
    /** Returns the zone in which the mouse was last seen. */
    Zone getCurrentZone() const noexcept                 { return mouse_zone; }
    
    void updateEditSelBox();
    Rectangle<int> getSelectionBounds();
    Rectangle<int> getPreviewBounds();

	/*************************************
	 *       ROTATE & RESIZE METHODS     *
	 *************************************/
    void createPreviewComponents( const MouseEvent& e );
    void rotatePreviewComponents( Point<int> mousePosition );
    void resizePreviewComponents( Point<int> mouseDelta );
	
	void rotateComponentSet();
	void resizeComponentSet();
	
	void flipSelectedSymbols( int axis );
	
protected:
	/*****************************
	 *       MOUSE CALLBACKS     *
	 *****************************/
    void mouseEnter (const MouseEvent&) override;
    void mouseMove (const MouseEvent&) override;
    void mouseDown (const MouseEvent&) override;
    void mouseDrag (const MouseEvent&) override;
    void mouseUp (const MouseEvent&) override;
    
    bool hitTest (int x, int y) override;
    void paint (Graphics&) override;
	
private:
	
	/**
	 * Stores an original and a copy version of a BaseComponent.
	 */
    class PreviewComponent {
    
    public:
        PreviewComponent(BaseComponent* newc, BaseComponent* src )
        {
            copy = newc;
            original = src;
        }
        
        ~PreviewComponent(){}
        
        BaseComponent* copy;
        BaseComponent* original;
        
    private:
        JUCE_LEAK_DETECTOR( PreviewComponent )

    };
	
	/** Components selected by this EditSelectionBox instance. */
    Array<ScoreComponent* >*      component_set = NULL;
	
    /** Symbols used to create preview components. */
    OwnedArray<Symbol >           original_symbols;
	
    /** Components used to preview the changes applied to the selected components. */
    OwnedArray<PreviewComponent > preview_components;
	
    /** Elements of component_set which are not of type BaseComponent. */
    Array<ScoreComponent* >       non_preview_components;
	
	/*******************************
	 *       LAYOUT  VARIABLES     *
	 *******************************/
    BorderSize<int> border_size;
	Rectangle<int>  original_bounds;
    Point<int>      previous_position;
    float           previous_theta = -111;
    float           accumulation_theta = 0;
    Zone       		mouse_zone;
	
    void updateMouseZone (const MouseEvent&);

    int min_width = 2;
    int min_height = 2;
    
    float scale_width = 1.0;
    float scale_height = 1.0;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (EditSelectionBox)
};

#endif
