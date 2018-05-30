#ifndef SmuflComponent_hpp
#define SmuflComponent_hpp

#include "JuceHeader.h"
#include <stdio.h>

#include "BaseComponent.h"

/**
 * Represents a SMuFL (Standard Music Font Layout) glyph.
 *
 * The SmuflComponent class encapsulates a DrawableText object
 * which is responsible for displaying the SMuFL glyph as a
 * character of the Bravura font.
 * The Bravura font is a implementation of SMuFL, which is only a
 * font specification.
 */
class SmuflComponent : public virtual BaseComponent {

public:
	
	/***************************************
	 *             CONSTRUCTORS            *
	 ***************************************/
	SmuflComponent();
	~SmuflComponent();
	
	/***************************************
	 *          GETTERS & SETTERS          *
	 ***************************************/
	inline String getUnicodeGlyph() { return glyph_code; }
	inline void setUnicodeGlyph(String unicodeGlyph) { glyph_code = unicodeGlyph; }
	
	inline string getSymbolTypeStr() const override { return "smufl"; }
	void addSymbolMessages(Symbol* symbol) override;
    void importFromSymbol(const Symbol &symbol) override;

	void paint(Graphics& g) override;
	void resized() override;
	
	void scaleScoreComponent(float scaledWidthRatio, float scaledHeightRatio) override;
	
	/********************************************
	 *           SMUFL METADATA METHODS         *
	 ********************************************/
	
	 /**
	  * Loads the content of the bravura metadata file
	  * into the smufl_metadata_json instance variable.
	  *
	  * @throws invalid_argument If the bravura_metadata.json file
	  *							 is not found at path /Library/Fonts/bravura/bravura_metadata.json.
	  */
	 static void loadMetadataFile();
	
	 /**
	  * Gets the bounding box of the glyph which unique name
	  * is passed in parameter.
	  *
	  * @param glyphName a string representing the unique name
	  *					 identifying a smugl glyph.
	  *
	  * @return 		 a rectangle with x and y coordinates set to 0, and which
	  *					 width and height corresponds to the width and height of
	  *					 the targeted smufl glyph.
	  */
	 static Rectangle<float> getBoundingBoxForGlyph(String glyphName);
	
	 /**
	  * Gets the width on height ratio for the gmyph which unique name
	  * is passed in parameter.
	  *
	  * @param glyphName a string representing the unique name
	  *					 identifying a smugl glyph.
	  *
	  * @return          a float representing the width on height ratio of
	  *					 the glyph's bounding box (which name is passed in parameter).
	  *
	  */
	 static float getWidthOnHeightRatioForGlyph(String glyphName);
	
private:
	static String SMUFL_METADATA_FILEPATH;
	static var smufl_metadata_json;
	
	/** The unicode code identifying the displayed glyph
	 * as defined in the SMuFL specification.
	 */
	String glyph_code;
	
	/** The unique name identifying the displayed glyph
	 * as defined in the SMuFL specification.
	 */
	String glyph_name;
	
	/** A graphic component used as a container to display the glyph */
	DrawableText smufl_glyph;
	
	//==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SmuflComponent)
};


#endif /* SmuflComponent_hpp */
