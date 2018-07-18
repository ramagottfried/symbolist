#pragma once

#include <stdio.h>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include "types.h"
#include "Symbol.h"
#include "TimePointArray.h"
#include "SortedStaves.hpp"
#include "osc_parser.h"

using namespace std;

#define PATH_TO_PALETTE_DEFAULT_ITEMS "./Resources/palette-default-items.osc"

/**
 * Describes the palette of symbols that can be drawn in the score.
 * Palette instances are model (non-graphic) objects : can be handled by the controllers
 * even if the graphic palette is not displayed in the window.
 */
class Palette {
	
public:
    inline Palette() {};
    inline ~Palette() {}
	
    inline void addDefaultItem(Symbol s) { default_items.push_back(s); }
    inline void addUserItem(Symbol s) { user_items.push_back(s); }
	
    inline Symbol* getPaletteDefaultItem(int i) { return &default_items[i]; }
    inline Symbol* getPaletteUserItem(int i) { return &user_items[i]; }
	
    inline int getPaletteNumDefaultItems() { return static_cast<int>(default_items.size()); }
    inline int getPaletteNumUserItems() { return static_cast<int>(user_items.size()); }
	
    inline int getSelectedItemIndex() { return selected_item; }
    inline void setSelectedItemIndex(int n) { selected_item = n; }
	
    /**
     * Gets the currently selected item in the palette.
     * It can be either a user-defined or a default item.
     *
     * @return a pointer to the currently selected symbol
     *         in the palette.
     */
	Symbol* getSelectedSymbol();
	
	/**
	 * Creates the default items in the palette.
	 *
	 * Normally, the default palette items are the
	 * circle, the square, the triangle and the text.
	 * Nevertheless, the default items in the palette
	 * could be set by the user in the user preferences.
	 *
	 */
	void createDefaultItems();
	
private:
	vector< Symbol > default_items;
    vector< Symbol > user_items;
	
    int selected_item = 0;
	
    //==============================================================================
    JUCE_LEAK_DETECTOR (Palette)
};

