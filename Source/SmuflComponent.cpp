#include "SmuflComponent.hpp"
#include "PaletteButton.hpp"

/////   WARNING : STEPS TO USE THE SMUFLCOMPONENT
/////
/////   - Download the Bravura font at https://www.smufl.org/fonts/
/////   - Install the Bravura font in your system.
/////   - Create the /Library/Fonts/bravura folder and copy the bravura_metadata.json
/////     in it (the file is to be found in the downloaded bravura zip file).

String SmuflComponent::SMUFL_METADATA_FILEPATH = "/Library/Fonts/bravura/bravura_metadata.json";

// Initializes the var object with the "undefined" value.
var SmuflComponent::smufl_metadata_json = var::undefined();

SmuflComponent::SmuflComponent()
{
	BaseComponent::BaseComponent();
	
	addAndMakeVisible(smufl_glyph);
	
	smufl_glyph.setFont(Font("Bravura", 100.0f, Font::plain), true);
	smufl_glyph.setJustification(Justification::centred);
	smufl_glyph.setColour(getCurrentColor());

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
	
	float glyphWidth = smufl_glyph.getFont().getStringWidth(glyph_code);
	float glyphHeight = smufl_glyph.getFont().getHeight();
	
	// if SmuflComponent is being rendered in a palette button, don't apply offset.
	if (dynamic_cast<PaletteButton* >(getParentComponent()) != NULL)
		setSize(glyphWidth, glyphHeight);
	else setSize(glyphWidth + 10, glyphHeight);
	
	smufl_glyph.setBoundingBox(RelativeParallelogram(Rectangle<float> (0, 0, getWidth(), getHeight())));
}

void SmuflComponent::resized()
{
	BaseComponent::resized();
}

void SmuflComponent::addSymbolMessages(Symbol* symbol)
{
	BaseComponent::addSymbolMessages(symbol);
	
	// Adds the glyph-code and glyph-name messages to the symbol.
	symbol->addMessage("/glyph-code", glyph_code.toStdString());
	symbol->addMessage("/glyph-name", glyph_name.toStdString());
}

void SmuflComponent::importFromSymbol(const Symbol &symbol)
{
	BaseComponent::importFromSymbol(symbol);
	
	// Extracts the glyph-code and the glyph-name from the symbol
	if (symbol.addressExists("/glyph-code"))
	{
		glyph_code = symbol.getMessage("/glyph-code").getString();
		smufl_glyph.setText(glyph_code);
	}
	
	if (symbol.addressExists("/glyph-name"))
		glyph_name = symbol.getMessage("/glyph-name").getString();
	
}

void SmuflComponent::scaleScoreComponent(float scaledWidthRatio, float scaledHeightRatio)
{
	/*
	 * Calculates and sets the new height
	 * based on the scaledHeightRatio and the
	 * current height of the SmuflComponent.
	 */
 	float newHeight = scaledHeightRatio * getHeight();
	
	/*
	 * Sets font height for the note glyph
	 * and adds the new font to smufl_glyph.
	 */
	Font noteFont = smufl_glyph.getFont();
	noteFont.setHeight(newHeight);
    smufl_glyph.setFont(noteFont, true);
	
	// Retrieves the glyph width with the new font height.
	float glyphWidth = smufl_glyph.getFont().getStringWidth(glyph_code);
	
	setSize(glyphWidth, newHeight);
	smufl_glyph.setBoundingBox(RelativeParallelogram(Rectangle<float> (0, 0, getWidth(), getHeight())));
	
	/* Moves the SmuflComponent position to place the glyph in the center
	 * of the palette button.
	 */
	PaletteButton* parent = dynamic_cast<PaletteButton* >(getParentComponent());
	if (parent != NULL)
	{
		float xCenter = (parent->getWidth() - getWidth()) / 2;
		setTopLeftPosition(xCenter, 0);
	}
	
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
		var noteQuarterUp = glyphBBoxes.getProperty(glyphName, var::undefined());
		if (!noteQuarterUp.isUndefined())
		{
			var nECoordinates = noteQuarterUp.getProperty("bBoxNE", var::undefined());
			var sWCoordinates = noteQuarterUp.getProperty("bBoxSW", var::undefined());
			
			if (nECoordinates.isUndefined() || sWCoordinates.isUndefined())
			{
				
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
