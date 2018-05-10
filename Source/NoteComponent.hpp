#ifndef NoteComponent_hpp
#define NoteComponent_hpp

#include "JuceHeader.h"
#include <stdio.h>
#include <locale>
#include <codecvt>

#include "BaseComponent.h"

class NoteComponent : public virtual BaseComponent {

public:
	
	/***************************************
	 *             CONSTRUCTORS            *
	 ***************************************/
	NoteComponent();
	
	/***************************************
	 *          GETTERS & SETTERS          *
	 ***************************************/
	inline String getUnicodeGlyph() { return unicode_glyph; }
	inline void setUnicodeGlyph(String unicodeGlyph) { unicode_glyph = unicodeGlyph; }
	
	inline string getSymbolTypeStr() const override { return "note"; }
	void addSymbolMessages(Symbol* symbol) override;
    void importFromSymbol(const Symbol &symbol) override;

	void paint(Graphics& g) override;
	void resized() override;
	
	void scaleScoreComponent(float scaledWidthRatio, float scaledHeightRatio) override;
	
	void setSymbolComponentColor(Colour c) override;
	
private:
	String unicode_glyph = String::fromUTF8("\uE1D2");
	DrawableText drawable_note;
	
	//==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (NoteComponent)
};


#endif /* NoteComponent_hpp */
