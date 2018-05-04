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

void Palette::createDefaultItems()
{
	try
	{
		OdotBundle* defaultItemsArray = OdotBundle::createOdotBundleFromFile(PATH_TO_PALETTE_DEFAULT_ITEMS);
		
		for (OdotAtom defaultItem : defaultItemsArray->getMessage("/default-items").getAtoms())
			if (defaultItem.isBundle())
				addDefaultItem(Symbol(new OdotBundle(defaultItem.getBundle())));
				
	}
	catch(invalid_argument& error)
	{
		// if an error occurs, let's create the default items in the code.
		cout << error.what() << endl;
		
		// Adds four default items in the palette.
		float symbolSize = 30.0;
		float symbolPos = 0.0;
		
		Symbol s1 = Symbol();
		s1.setTypeXYWH("text", symbolPos, symbolPos, 20 , 20);
		addDefaultItem(s1);
		
		Symbol s2 = Symbol();
		s2.setTypeXYWH("circle", symbolPos, symbolPos, symbolSize, symbolSize);
		addDefaultItem(s2);
		
		Symbol s3 = Symbol();
		s3.setTypeXYWH("rectangle", symbolPos, symbolPos, symbolSize, symbolSize);
		addDefaultItem(s3);
		
		Symbol s4 = Symbol();
		s4.setTypeXYWH("triangle", symbolPos, symbolPos, symbolSize, symbolSize);
		addDefaultItem(s4);
		
	}
}
