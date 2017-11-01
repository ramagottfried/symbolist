#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "types.h"
#include "SymbolistHandler.h"


class PageComponent; // forward declaration of subclass


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


class SymbolistComponent : public Component
{
public:
    
    virtual String getSymbolTypeStr() const { return " ??? " ; } ; // every component defines its type

    virtual PageComponent* getPageComponent();
    virtual SymbolistHandler* getSymbolistHandler();
    SymbolistMainComponent* getMainComponent();
    
    UI_EditType getMainMouseMode();
    UI_DrawType getMainDrawMode();
    virtual void selectComponent();
    virtual void deselectComponent();
    bool componentSelected();
    
    const size_t    getNumSubcomponents( );
    SymbolistComponent* getSubcomponent( int i );
    SymbolistComponent* getSubcomponentByID( String& id );

    virtual void    addSubcomponent( SymbolistComponent *c );
    virtual void    removeSubcomponent( SymbolistComponent *c );
    virtual void    clearAllSubcomponents();
    
    Point<int> positionRelativeTo(SymbolistComponent* to);
    void mouseDownSelection( const MouseEvent& event );
    virtual bool intersectRect( Rectangle<int> rect);

    virtual void setEditMode(bool val) {}
    virtual bool isInEditMode(){ return false; }

    virtual void updateRelativeAttributes() {} // << virtual to avoid class type checking later
    
    virtual void rotateScoreComponent(float theta, float ax, float ay){}
    virtual void h_flip(float ax, float ay) {}
    virtual void v_flip(float ax, float ay) {}
    

protected :
    
    bool is_selected = false;
    Array<SymbolistComponent*>     subcomponents;
    
};
