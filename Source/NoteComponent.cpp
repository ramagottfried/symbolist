#include "NoteComponent.hpp"
#include "PaletteButton.hpp"

NoteComponent::NoteComponent()
{
	BaseComponent::BaseComponent();
	addAndMakeVisible(drawable_note);
	
	drawable_note.setFont(Font("Bravura", 100.0f, Font::plain), true);
	drawable_note.setText(unicode_glyph);
	drawable_note.setJustification(Justification::centred);
	drawable_note.setColour(getCurrentColor());
}

void NoteComponent::paint(Graphics& g)
{
	BaseComponent::paint(g);
	
	int unicodeGlyphHeight = drawable_note.getFont().getHeight();
	int unicodeGlyphWidth = drawable_note.getFont().getStringWidth(unicode_glyph);
	
	// if NoteComponent is being rendered in a palette button, don't apply offset
	if (dynamic_cast<PaletteButton* >(getParentComponent()) != NULL)
		setSize(unicodeGlyphWidth, unicodeGlyphHeight);
	else setSize(unicodeGlyphWidth + 10, unicodeGlyphHeight);
	
	drawable_note.setBoundingBox(RelativeParallelogram(Rectangle<float> (0, 0, getWidth(), getHeight())));
}

void NoteComponent::resized()
{
	BaseComponent::resized();
	
	int unicodeGlyphHeight = drawable_note.getFont().getHeight();
	int unicodeGlyphWidth = drawable_note.getFont().getStringWidth(unicode_glyph);
	
	// if NoteComponent is being rendered in a palette button, don't apply offset
	if (dynamic_cast<PaletteButton* >(getParentComponent()) != NULL)
		setSize(unicodeGlyphWidth, unicodeGlyphHeight);
	else setSize(unicodeGlyphWidth + 10, unicodeGlyphHeight);
	
	drawable_note.setBoundingBox(RelativeParallelogram(Rectangle<float> (0, 0, getWidth(), getHeight())));
}

void NoteComponent::addSymbolMessages(Symbol* symbol)
{
	BaseComponent::addSymbolMessages(symbol);
	symbol->addMessage("/unicode-glyph", unicode_glyph.toStdString());
}

void NoteComponent::importFromSymbol(const Symbol &symbol)
{
	BaseComponent::importFromSymbol(symbol);
	
	if (symbol.addressExists("/unicode-glyph"))
	{
		unicode_glyph = symbol.getMessage("/unicode-glyph").getString();
		drawable_note.setText(unicode_glyph);
	}
	
}

void NoteComponent::scaleScoreComponent(float scaledWidthRatio, float scaledHeightRatio)
{
 	float newHeight = scaledHeightRatio * getHeight();
    float newWidth = scaledWidthRatio * getWidth();
	
	setSize(newWidth, newHeight);
	
	int verticalOffset = 40;
	
	Font noteFont = drawable_note.getFont();
    noteFont.setHeightWithoutChangingWidth( newHeight + verticalOffset );
	
//    float currentNoteWidth = noteFont.getStringWidthFloat(unicode_glyph);
//
//    float currentHFontScale = noteFont.getHorizontalScale();
//    float targetHFontScale = newWidth / currentNoteWidth;
//
//	  float newHFontScale = currentHFontScale * targetHFontScale;
//
//    noteFont.setHorizontalScale( newHFontScale );
	
    drawable_note.setFont(noteFont, true);
	setTopLeftPosition(getX(), -(verticalOffset / 4));
}

void NoteComponent::setSymbolComponentColor(Colour c)
{
	BaseComponent::setSymbolComponentColor(c);
	drawable_note.setColour(c);
}
