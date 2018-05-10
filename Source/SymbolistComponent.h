#pragma once

#ifndef SymbolistComponent_h
#define SymbolistComponent_h

#include "JuceHeader.h"
#include "types.h"
#include "SymbolistHandler.h"

class PageComponent; // forward declaration of subclass

class SymbolistComponent : public Component {

public:
    virtual string getSymbolTypeStr() const { return string(" ??? "); } ; // every component defines its type

    virtual PageComponent* getPageComponent();
    virtual SymbolistHandler* getSymbolistHandler();
    SymbolistMainComponent* getMainComponent();
    
    UI_EditType getMainMouseMode();
    UI_DrawType getMainDrawMode();
	
    virtual void setEditMode(bool val) {}
    virtual bool isInEditMode() { return false; }
	
    virtual inline void setSymbolComponentColor(Colour c) { sym_color = c; }
	
protected :
    
    Colour sym_color = Colours::black;
	
};

#endif
