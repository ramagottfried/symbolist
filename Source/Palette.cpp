#include "Palette.hpp"

Symbol* Palette::getSelectedSymbol()
{
    int defaultItemsCount = getPaletteNumDefaultItems();
    int indexOfSelectedItem = getSelectedItemIndex();
	
    if (indexOfSelectedItem < defaultItemsCount)
        return getPaletteDefaultItem(indexOfSelectedItem);
    else
        return getPaletteUserItem(indexOfSelectedItem - defaultItemsCount);

}
