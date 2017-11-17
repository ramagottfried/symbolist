
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistComponent.h"
#include "PathHandleComponent.h"
#include "Symbol.h"

// adapted from JUCE ResizableBoarderComponent
class EditSelectionBox  : public Component
{
public:
    //==============================================================================
    /** Creates a resizer.
     
     Pass in the target component which you want to be resized when this one is
     dragged.
     
     The target component will usually be a parent of the resizer component, but this
     isn't mandatory.
     
     Remember that when the target component is resized, it'll need to move and
     resize this component to keep it in place, as this won't happen automatically.
     
     If the constrainer parameter is not a nullptr, then this object will be used to
     enforce limits on the size and position that the component can be stretched to.
     Make sure that the constrainer isn't deleted while still in use by this object.
     
     @see ComponentBoundsConstrainer
     */
    EditSelectionBox (Array<SymbolistComponent*>* const selected_component_array );

    
    ~EditSelectionBox();
    
    
    void setBorderThickness (const BorderSize<int>& newBorderSize);
    
    BorderSize<int> getBorderThickness() const;
    
    
    //==============================================================================
    /** Represents the different sections of a resizable border, which allow it to
     resized in different ways.
     */
    class Zone
    {
    public:
        //==============================================================================
        enum Zones
        {
            centre  = 0,
            left    = 1,
            top     = 2,
            right   = 4,
            bottom  = 8
        };
        
        //==============================================================================
        /** Creates a Zone from a combination of the flags in \enum Zones. */
        explicit Zone (int zoneFlags) noexcept;
        
        Zone() noexcept;
        Zone (const Zone&) noexcept;
        Zone& operator= (const Zone&) noexcept;
        
        bool operator== (const Zone&) const noexcept;
        bool operator!= (const Zone&) const noexcept;
        
        //==============================================================================
        /** Given a point within a rectangle with a resizable border, this returns the
         zone that the point lies within.
         */
        static Zone fromPositionOnBorder (const Rectangle<int>& totalSize,
                                          const BorderSize<int>& border,
                                          Point<int> position);
        
        /** Returns an appropriate mouse-cursor for this resize zone. */
        MouseCursor getMouseCursor() const noexcept;
        
        /** Returns true if dragging this zone will move the enire object without resizing it. */
        bool isDraggingWholeObject() const noexcept     { return zone == centre; }
        /** Returns true if dragging this zone will move the object's left edge. */
        bool isDraggingLeftEdge() const noexcept        { return (zone & left) != 0; }
        /** Returns true if dragging this zone will move the object's right edge. */
        bool isDraggingRightEdge() const noexcept       { return (zone & right) != 0; }
        /** Returns true if dragging this zone will move the object's top edge. */
        bool isDraggingTopEdge() const noexcept         { return (zone & top) != 0; }
        /** Returns true if dragging this zone will move the object's bottom edge. */
        bool isDraggingBottomEdge() const noexcept      { return (zone & bottom) != 0; }
        
        /** Resizes this rectangle by the given amount, moving just the edges that this zone
         applies to.
         */
        template <typename ValueType>
        Rectangle<ValueType> resizeRectangleBy (Rectangle<ValueType> original,
                                                const Point<ValueType>& distance) const noexcept
        {
            if (isDraggingWholeObject())
                return original + distance;
            
            if (isDraggingLeftEdge())   original.setLeft (jmin (original.getRight(), original.getX() + distance.x));
                if (isDraggingRightEdge())  original.setWidth (jmax (ValueType(), original.getWidth() + distance.x));
                    if (isDraggingTopEdge())    original.setTop (jmin (original.getBottom(), original.getY() + distance.y));
                        if (isDraggingBottomEdge()) original.setHeight (jmax (ValueType(), original.getHeight() + distance.y));
                            
                            return original;
        }
        
        /** Returns the raw flags for this zone. */
        int getZoneFlags() const noexcept               { return zone; }
        
    private:
        //==============================================================================
        int zone;
    };
    
    /** Returns the zone in which the mouse was last seen. */
    Zone getCurrentZone() const noexcept                 { return mouseZone; }
    
    void updateEditSelBox();
    Rectangle<int> getSelectionBounds();
    Rectangle<int> getPreviewBounds();

    void flipSelectedSymbols( int axis );

    
protected:
    void paint (Graphics&) override;
    void mouseEnter (const MouseEvent&) override;
    void mouseMove (const MouseEvent&) override;
    void mouseDown (const MouseEvent&) override;
    void mouseDrag (const MouseEvent&) override;
    void mouseUp (const MouseEvent&) override;
    bool hitTest (int x, int y) override;
    
private:
    Array<SymbolistComponent*>  *component_set;
    
    OwnedArray<Symbol> original_symbols;
    OwnedArray<SymbolistComponent> preview_components;
    
    BorderSize<int> borderSize;

    Zone mouseZone;
    Point<int> prev_pos;
    float m_prev_theta = -111;
    float m_accum_theta = 0;
    
    Rectangle<int> original_bounds;
    
    void updateMouseZone (const MouseEvent&);

    int m_minw = 2;
    int m_minh = 2;
    
    float m_scale_w = 1.0;
    float m_scale_h = 1.0;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (EditSelectionBox)
};
