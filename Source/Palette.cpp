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
		/* Bypassing file reading for now because reading utf-8 characters
		 * from a text file doesn't work.
		 * The Bravura glyphs are not well rendered.
		 */
		throw invalid_argument("Bypassing palette-default-items.osc file reading.");
		
//		OdotBundle* defaultItemsArray = OdotBundle::createOdotBundleFromFile(PATH_TO_PALETTE_DEFAULT_ITEMS);
//
//		for (OdotAtom defaultItem : defaultItemsArray->getMessage("/default-items").getAtoms())
//			if (defaultItem.isBundle())
//				addDefaultItem(Symbol(new OdotBundle(defaultItem.getBundle())));
		
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
		
		Symbol s5 = Symbol();
		s5.setTypeXYWH("smufl", symbolPos, symbolPos, symbolSize, symbolSize);
		s5.addMessage("/glyph-code", String::fromUTF8("\uE008").toStdString());
		s5.addMessage("/glyph-name", "systemDividerLong");
		addDefaultItem(s5);
		
		Symbol s6 = Symbol();
		s6.setTypeXYWH("smufl", symbolPos, symbolPos, symbolSize, symbolSize);
		s6.addMessage("/glyph-code", String::fromUTF8("\uE1D3").toStdString());
		s6.addMessage("/glyph-name", "noteHalfUp");
		addDefaultItem(s6);
		
		Symbol s7 = Symbol();
		s7.setTypeXYWH("smufl", symbolPos, symbolPos, symbolSize, symbolSize);
		s7.addMessage("/glyph-code", String::fromUTF8("\uE1D5").toStdString());
		s7.addMessage("/glyph-name", "noteQuarterUp");
		addDefaultItem(s7);
		
		Symbol s8 = Symbol();
		s8.setTypeXYWH("smufl", symbolPos, symbolPos, symbolSize, symbolSize);
		s8.addMessage("/glyph-code", String::fromUTF8("\uE1D7").toStdString());
		s8.addMessage("/glyph-name", "note8thUp");
		addDefaultItem(s8);
		
		Symbol s9 = Symbol();
		s9.setTypeXYWH("smufl", symbolPos, symbolPos, symbolSize, symbolSize);
		s9.addMessage("/glyph-code", String::fromUTF8("\uE1D9").toStdString());
		s9.addMessage("/glyph-name", "note16thUp");
		addDefaultItem(s9);
		
		Symbol s10 = Symbol();
		s10.setTypeXYWH("smufl", symbolPos, symbolPos, symbolSize, symbolSize);
		s10.addMessage("/glyph-code", String::fromUTF8("\uE904").toStdString());
		s10.addMessage("/glyph-name", "mensuralFclefPetrucci");
		addDefaultItem(s10);
	}
}
