#pragma once

#include <vector>
#include "JuceHeader.h"
#include "types.h"
#include "Symbol.h"
#include "TimePointArray.h"
#include "SortedStaves.hpp"

#include "SVGFileIO.hpp"

#include "OdotHash.hpp"

using namespace std;

//============================
// SCORE
//============================

/**
 * A sorter for the symbols in the score, based on time value comparison.
 */
struct ScoreSorter
{
    bool operator() (const std::unique_ptr<Symbol>& a, const std::unique_ptr<Symbol>& b)
    {
        auto a_t = a->getTime();
        auto b_t = b->getTime();
        return (a_t < b_t);
    }
};

class Score {

public:
    
    /***************************************
     *             CONSTRUCTORS            *
     ***************************************/
    Score();
    Score(const Score& src);
    Score(const OdotBundle_s& s_bundle) ;
    ~Score();
    

    
    /**********************************************
     *             GETTERS AND SETTERS            *
     **********************************************/
    /**
     * Sets default stave sorting function and pixel to time value
     *
     */
    void setDefaults();

    
    inline TimePointArray* getTimePoints() { return &time_points; };
    
    /**
     * Gets the count of symbols contained in the score.
     *
     * @return the size of the score_symbols vector.
     */
    size_t getSize();
    
    /**
     * Gets the symbol at index n in the score.
     *
     * @param n             the index of the symbol to retrieve
     *                      in the score.
     *
     * @throws length_error If n is bigger than the score size.
     *
     * @return              a pointer to the symbol at
     *                      index n in the score.
     */
    Symbol* getSymbol(int n);
	
    /**
     * Gets the position of a symbol in the score.
	 *
	 * @param symbol a reference to the targeted symbol.
	 *
	 * @returns      the index the symbol in parameter in the score's vector
	 *               of symbols, or -1 if the symbol is not in the score.
	 */
    int getSymbolPosition(Symbol* symbol);
    
    /**
     * Creates a new symbol in the score.
     *
     * @return a pointer to the newly created symbol.
     */
    Symbol* createSymbol();
    
    /**
     * Adds a new symbol to the score.
     *
     * Actually, a new symbol is created by copying
     * the one in parameter, then it is added to the score.
     * If the symbol in parameter already exists in the score
     * then the symbol is updated with the values of the incoming
     * symbol, and a pointer to the currently existing symbol
     * in the score is returned.
     *
     * @param symbol a pointer to the symbol which will be copied
     *               to create a new symbol in the score.
     *
     * @return       a pointer to the newly created symbol.
     */
    Symbol* addSymbol(Symbol* symbol);
    
    
    /**
     * Makes a copy of a symbol an adds it to the score.
     *
     *
     * @param symbol a pointer to the symbol which will be copied
     *               to create a new symbol in the score.
     *
     * @return       a pointer to the newly created symbol.
     */
    Symbol* addDuplicateSymbol(Symbol* symbol);
    
    /**
     * Removes a symbol from the score.
     *
     * Looks for the symbol passed as an argument in the score,
     * and removes it if it exists.
     *
     * @param symbol            a pointer to the symbol to be removed
     *                          from the score.
     *
     * @throws invalid_argument If the pointer passed as an argument is
     *                          <code>NULL</code> or if the referenced
     *                          symbol is not in the score.
     *
     * @throws logic_error      If the method is called while the score
     *                          is empty (there are no symbols to remove).
     */
    void removeSymbol(Symbol* symbol);
	
	/**
	 * Clears all symbols for this Score instance.
	 *
	 * Removes all elements from the vector of symbols, the array
	 * of time points, and the list of sorted staves.
	 */
    void removeAllSymbols();
	
	/**
	 * Clears all symbols for this Score instance, then re-populates it.
	 *
	 * @param s_bundle a serialized odot bundle containing symbols to
	 *                 re-populate the score.
	 */
    void importReplaceScore( const OdotBundle_s& s_bundle );
	
    /**
     * Iterates over all OSC messages contained in s_bundle, and creates
     * a new symbol in the score for each OSC bundle found in a message.
     * More precisely, an OSC bundle can be stored as a full bundle structure
     * or as a string in the messages of s_bundle.
     *
     * @param s_bundle a serialized OSC bundle containing other OSC bundles
     *				   in its messages.
     *
     */
    void importSymbols( const OdotBundle_s& s_bundle );

    
    /**
     * Trigger rebuilding of Timepoint array and Stave sorting.
     *
     */
    void buildTimeLookups();
    
    void addSymbolTimePoints( Symbol* s );
    void removeSymbolTimePoints( Symbol* s );
    
    OdotBundle_s getSymbolsAtTime( float t );
    
    OdotBundle_s getScoreBundle_s();
    
    string getJSON();

    string getSVG();
    
    
    /**
     * Imports the Score from an SVG file on disk.
     *
     *
     * @param filename          name of file to write, including path
     *
     * @throws invalid_argument (nothing yet)
     *
     * @throws logic_error      (nothing yet)
     */
    inline int importSVG( const char * filename )
    {
        SVGFileIO svg;
        svg.read( filename );
        
        return 0;
    }
    
    /**
     * Exports the Score from to SVG file on disk.
     *
     *
     * @param filename          name of file to write, including path
     *
     * @throws invalid_argument (nothing yet)
     *
     * @throws logic_error      (nothing yet)
     */
    inline int exportSVG( const char * filename )
    {
        SVGFileIO svg;
        svg.write( score_symbols, filename );
        
        return 0;
    }
    
    const Array<Symbol*> getSymbolsByValue(const string& address, const string& value);
    
    const StringArray getStaves();
    void addStaff( Symbol* s );
    
    void updateStaves(Symbol* moved_stave);
    void updateStavesAndTimepoints();
    Symbol* getStaveAtTime( float time );
    const Symbol* getStaveByID( const string& id );

    OdotBundle_s getDurationBundle();
    
    int getNameCount( string& name );
    
    bool idExists( string& searchedId );
	
	void print() const;
	
private:

    OdotBundle                  m_score;
    
    OdotBundleHash              m_symbol_table;
    OdotBundleHash              m_type_selector;

	vector< unique_ptr<Symbol> > score_symbols;
    TimePointArray              time_points;
    
    SortedStaves                staves;
    ScoreSorter                 score_sorter;
	
    //==============================================================================
    JUCE_LEAK_DETECTOR (Score)
};








