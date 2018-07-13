#include "SmuflComponent.hpp"
#include "PaletteButton.hpp"
#include "PageComponent.h"

/////   WARNING : STEPS TO USE THE SMUFLCOMPONENT
/////
/////   - Download the Bravura font at https://www.smufl.org/fonts/
/////   - Install the Bravura font in your system.
/////   - Create the /Library/Fonts/bravura folder (where first slash is the system root)
/////     and copy the bravura_metadata.json in it (the file is in the downloaded bravura zip file).

String SmuflComponent::SMUFL_METADATA_FILEPATH = "/Library/Fonts/bravura/bravura_metadata.json";

// Initializes the var object with the "undefined" value.
var SmuflComponent::smufl_metadata_json = var::undefined();

SmuflComponent::SmuflComponent()
{
	smufl_glyph.setFont(Font("Bravura", font_height, Font::plain), true);
	smufl_glyph.setJustification(Justification::centred);
	smufl_glyph.setColour(getCurrentColor());
	
	addAndMakeVisible(smufl_glyph);

	try {
	
  		/* Loads the smufl metadata file content into smufl_metadata_json
	 	 * if the instance variable has the value var::undefined.
	 	 */
		if (smufl_metadata_json.isUndefined())
			loadMetadataFile();
		
	} catch (invalid_argument &error) {
		cout << error.what() << endl;
	}
	
}

SmuflComponent::~SmuflComponent()
{
	/* Removes all DynamicObject instances created
	 * when json metadata was parsed.
	 */
	if (!smufl_metadata_json.isUndefined())
		smufl_metadata_json = var::undefined();
}

void SmuflComponent::paint(Graphics& g)
{
	BaseComponent::paint(g);
	
	// Sets color for the SmuflComponent and its inner drawable.
	g.setColour(getCurrentColor());
	smufl_glyph.setColour(getCurrentColor());
	
}

void SmuflComponent::resized()
{
	BaseComponent::resized();
	smufl_glyph.setBoundingBox(Parallelogram<float>(Rectangle<float> (0, 0, getWidth(), getHeight())));
}

void SmuflComponent::addSymbolMessages(Symbol* symbol)
{
	BaseComponent::addSymbolMessages(symbol);
	
	// Adds the glyph-code, glyph-name and font-height messages to the symbol.
	symbol->addMessage("/glyph/code", glyph_code.toStdString());
	symbol->addMessage("/glyph/name", glyph_name.toStdString());
	symbol->addMessage("/font/height", font_height);
}

void SmuflComponent::importFromSymbol(const Symbol &symbol)
{
	// Extracts the glyph-code and the glyph-name from the symbol
	if (symbol.addressExists("/glyph/code"))
	{
		glyph_code = symbol.getMessage("/glyph/code").getString();
		smufl_glyph.setText(glyph_code);
	}
	
	if (symbol.addressExists("/glyph/name"))
		glyph_name = symbol.getMessage("/glyph/name").getString();
	
	// Updates the font height of smufl_glyph.
	if (symbol.addressExists("/font/height"))
	{
		font_height = symbol.getMessage("/font/height").getFloat();
		Font newFont = smufl_glyph.getFont();
		newFont.setHeight(font_height);
		
		smufl_glyph.setFont(newFont, true);
	}
	
	BaseComponent::importFromSymbol(symbol);
	
}

void SmuflComponent::scaleScoreComponent(float scaledWidthRatio, float scaledHeightRatio)
{


	/* Moves the SmuflComponent position to place the glyph in the center
	 * of the palette button.
	 */
	PaletteButton* parent = dynamic_cast<PaletteButton* >(getParentComponent());
	if (parent != NULL)
	{
		float paletteButtonHeight = 50.0f;
		
		/*
		 * Sets font height for the note glyph
		 * and adds the new font to smufl_glyph.
		 */
		Font noteFont = smufl_glyph.getFont();
		noteFont.setHeight(paletteButtonHeight);
		
		// Retrieves the glyph width with the new font height.
		float glyphWidth = noteFont.getStringWidth(glyph_code);
		
		setSize(glyphWidth, paletteButtonHeight);
		
		float xCenter = (abs(parent->getWidth() - getWidth())) / 2;
		float yCenter = -10; // -17, Offset to center the glyphs in palette buttons
		setTopLeftPosition(xCenter, yCenter);
	}
	else
	{
		/*
		 * Calculates and sets the new height based on the scaledHeightRatio and the
		 * current height of the SmuflComponent.
		 */
		float newHeight = scaledHeightRatio * getHeight();
		font_height = newHeight; // Stores the new value of font_height
		
		/*
		 * Sets font height for the note glyph
		 * and adds the new font to smufl_glyph.
		 */
		Font noteFont = smufl_glyph.getFont();
		
		// Multiply three times the height to obtain a good size in the palette button
		noteFont.setHeight(newHeight);
		smufl_glyph.setFont(noteFont, true);
		
		// Retrieves the glyph width with the new font height.
		float glyphMargin = 8.0;
		float glyphWidth = smufl_glyph.getFont().getStringWidth(glyph_code) + glyphMargin;
		
		setSize(glyphWidth, newHeight);
		
	}
	
}

void SmuflComponent::setBoundsFromSymbol( float x, float y, float w, float h )
{
	float glyphWidth = smufl_glyph.getFont().getStringWidth(glyph_code);
	float glyphHeight = smufl_glyph.getFont().getHeight();

	DEBUG_FULL("Glyph width = "  << glyphWidth << ", glyph height = " << glyphHeight << endl)

	// if SmuflComponent is being rendered in a palette button, don't apply offset.
	if (dynamic_cast<PaletteButton* >(getParentComponent()) != NULL)
			setBounds(x, y, glyphWidth, glyphHeight);
	else setBounds(x, y, glyphWidth + 8, glyphHeight);
}

void SmuflComponent::mouseAddClick(const MouseEvent& event)
{
	// Delegates the call to page component on click.
	ScoreComponent* parentComponent = dynamic_cast<ScoreComponent* >(getParentComponent());
	if (parentComponent != NULL)
		parentComponent->mouseAddClick(event);
	
}

void SmuflComponent::loadMetadataFile()
{
	/* Creates a file input stream to read the content
	 * of the smufl metadata file.
	 */
	FileInputStream smuflMetadataStream(File(SMUFL_METADATA_FILEPATH.toStdString()));

	if(smuflMetadataStream.failedToOpen())
		throw invalid_argument("Invalid path to smufl metadata file.");

	smufl_metadata_json = JSON::parse(smuflMetadataStream);
}

Rectangle<float> SmuflComponent::getBoundingBoxForGlyph(String glyphName)
{
	if (smufl_metadata_json.isUndefined())
		loadMetadataFile();
	
	var glyphBBoxes = smufl_metadata_json.getProperty("glyphBBoxes", var::undefined());
	if (!glyphBBoxes.isUndefined())
	{
		var targetGlyph = glyphBBoxes.getProperty(glyphName, var::undefined());
		if (!targetGlyph.isUndefined())
		{
			var nECoordinates = targetGlyph.getProperty("bBoxNE", var::undefined());
			var sWCoordinates = targetGlyph.getProperty("bBoxSW", var::undefined());
			
			if (nECoordinates.isUndefined() || sWCoordinates.isUndefined())
			{
				// Throws something. NYI.
			}
			else
			{
				float width = nECoordinates[0].toString().getFloatValue() - sWCoordinates[0].toString().getFloatValue();
				float height = nECoordinates[1].toString().getFloatValue() - sWCoordinates[1].toString().getFloatValue();
				
				return Rectangle<float>(0, 0, width, height);
			}
			
		}
		
	}
	
	return Rectangle<float>(0, 0, 0, 0);
}

float SmuflComponent::getWidthOnHeightRatioForGlyph(String glyphName)
{
	Rectangle<float> glyphBBox = SmuflComponent::getBoundingBoxForGlyph(glyphName);
	return glyphBBox.getWidth() / glyphBBox.getHeight();
	
}
