#ifndef SymbolistPalette_hpp
#define SymbolistPalette_hpp

#include <stdio.h>
#include "../JuceLibraryCode/JuceHeader.h"
#include <vector>
#include "types.h"
#include "Symbol.h"
#include "TimePointArray.h"
#include "SortedStaves.hpp"

/**
 * Describes the palette of symbols that can be drawn in the score.
 * Palette instances are model (non-graphic) objects : can be handle by the SymbolistMainComponent
 * even if the graphic palette is not displayed in the window.
 */
class Palette
{

    OwnedArray<Symbol> default_items;
    OwnedArray<Symbol> user_items;
    
    int selected_item = 0;

public:
    
    inline Palette() {};
    inline ~Palette() {}
    
    inline void addDefaultItem( Symbol *s ) { default_items.add(s); }
    inline void addUserItem( Symbol *s ) { user_items.add(s); }
    inline Symbol* getPaletteDefaultItem( int i ) { return default_items[i] ; }
    inline Symbol* getPaletteUserItem( int i ) { return user_items[i] ; }
    inline int getPaletteNumDefaultItems() { return static_cast<int>( default_items.size() ) ; }
    inline int getPaletteNumUserItems() { return static_cast<int>( user_items.size() ) ; }
    inline int getSelectedItem() { return selected_item ; }
    inline void setSelectedItem(int n) { selected_item = n; }
    
};

#endif /* SymbolistPalette_hpp */
