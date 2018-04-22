#include "Score.h"

#include <iostream>
#include <algorithm>
#include <vector>

Score::Score()
{
    DEBUG_FULL("Score instance address: " << this << ", score size: " << score_symbols.size() << endl);
    
    // Sets the score_ptr reference for time_points instance variable.
    time_points.setScore(this);
}

Score::Score(Score& src)
{
    // Sets the score_ptr reference for time_points instance variable.
    time_points.setScore(this);
    
    for(auto it = src.score_symbols.begin(); it != src.score_symbols.end(); it++)
    {
        score_symbols.push_back(std::unique_ptr<Symbol>(new Symbol( it->get() ) ) );
        addStaff(score_symbols.back().get());
    }
    
    updateStavesAndTimepoints();
    
    DEBUG_FULL("Copying score of address " << this << " and size " << score_symbols.size()
    			<< ", owning " << staves.size() << " staves " << endl);
}

Score::Score( const OdotBundle_s& s_bundle  )
{
    importSymbols( s_bundle );
}

Score::~Score()
{

}

void Score::print() const
{
    int count = 1;
    for (auto it = score_symbols.begin(); it != score_symbols.end(); it++)
    {
        DEBUG_INLINE("Symbol n° " << count << endl);
        (*it)->print();
        count++;
    }
}

/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::removeAllSymbols()
{
    score_symbols.clear();
    time_points.getSymbolTimePoints().clear();
    staves.clear();
}

/******************************************
 * Creates a new empty symbol in the score
 ******************************************/
Symbol* Score::createSymbol()
{
    return addSymbol(NULL);
}

/***********************************
 * Add a new Symbol in the Score
 ***********************************/
Symbol* Score::addSymbol(Symbol* symbol)
{
    DEBUG_FULL(symbol << endl );

    // Calls symbol's empty constructor if reference is NULL.
    if (symbol == NULL)
    {
        score_symbols.push_back(unique_ptr<Symbol>(new Symbol()));
        return score_symbols.back().get();
    }
    
    /* If symbol id exists in score, then union the incoming values with the current values
     * and return the symbol reference.
     */
    string id = symbol->getID();
    DEBUG_FULL("checking for " << id << endl );

    if( id == "" )
        DEBUG_FULL("possible error: this symbol has no id text " << endl );
    
    auto iteratorToSymbol = find_if(score_symbols.begin(),
                                    score_symbols.end(),
                                    [id]( unique_ptr<Symbol>& ptrToSymbol )
                                    {
                                        return (ptrToSymbol->getID() == id );
                                    });
    
    if (iteratorToSymbol != score_symbols.end())
    {
        (*iteratorToSymbol)->unionWith( symbol, true );
        DEBUG_FULL("iteratorToSymbol " << endl );
        return (*iteratorToSymbol).get();
    }
    
    // make copy and add to symbol vector
    score_symbols.push_back( unique_ptr<Symbol>( new Symbol(*symbol) ) );
    

    /* Retrieves the last inserted symbol's reference
     * before sorting the score
     */
    Symbol* lastInsertedSymbol = score_symbols.back().get();
    DEBUG_FULL("lastInsertedSymbol" << lastInsertedSymbol << endl );

    // Calls the sort function to properly insert the new symbol.
    sort(score_symbols.begin(), score_symbols.end(), score_sorter);
    
    /* lastInsertedSymbol is added to staves
     * only if it is of type staff.
     */
    DEBUG_FULL("attempt to add staff " << lastInsertedSymbol << endl );
    bool newstaff = staves.addStaff(lastInsertedSymbol);
    
    /* if lastInsertedSymbol is linked to a staff
       then time points are added to array.
     */
    time_points.addSymbolTimePoints(lastInsertedSymbol);

    if (newstaff)
        for (auto it = score_symbols.begin(); it != score_symbols.end(); it++)
            /* This should look up by name not nameID, in the
             * timepoints the staves should be combined...
             * Although then I guess the types of clef could change?
             * Leaving as id for now, but this is unintuitive to set from outside the editor.
             */
            if ((*it)->getSaff() == lastInsertedSymbol->getID())
                time_points.addSymbolTimePoints((*it).get());
    
    return lastInsertedSymbol;
}

/***********************************
 * Add a new Symbol in the Score duplicating the input symbol (but with a unique id)
 ***********************************/
Symbol* Score::addDuplicateSymbol(Symbol* symbol)
{
    string name = symbol->getName();
    string id = symbol->getID();
    
    vector<string> ids;
    for( auto it = score_symbols.begin(); it != score_symbols.end(); it++ )
    {
        if( (*it)->getName() == name )
            ids.emplace_back( (*it)->getID() );
    }
    
    size_t count = ids.size();
    string nextID = name+"/"+to_string(count++);
    
    while( find( ids.begin(), ids.end(), nextID ) != ids.end() )
    {
        nextID = name+"/"+to_string(count++);
    }
    
    symbol->addMessage("/id", nextID );
    //DEBUG_FULL(nextID << endl);
    return addSymbol(symbol);
}


/***********************************
 * Removes a Symbol from the Score
 ***********************************/
void Score::removeSymbol(Symbol* symbol)
{
    /* For now, prints a message.
     * Could be better to throw an exception.
     */
    if (symbol == NULL)
        throw invalid_argument("Symbol pointer argument is NULL.");
    
    // If score is not empty.
    if (!score_symbols.empty())
    {
        // Then looks for symbol in score_symbols.
        auto iteratorToSymbol = find_if(score_symbols.begin(),
                                        score_symbols.end(),
                                        [symbol](unique_ptr<Symbol>& ptrToSymbol) {
                                            return ptrToSymbol.get() == symbol;
                                        });
        /* If returned iterator is different from end()
         * then symbol is in score_symbols.
         */
        if (iteratorToSymbol != score_symbols.end())
        {
            staves.removeStaff(symbol);
            score_symbols.erase(iteratorToSymbol);
        }
        else throw invalid_argument("Symbol pointer is not among score's symbols.");
    }
    else throw logic_error("Attempting to remove a symbol while score is empty.");
    
}

/***********************************
 * Create Single Bundle from Score Bundles
 ***********************************/
OdotBundle_s Score::getScoreBundle_s()
{
    // for now merging flat array into bundle and then serializing
    // probalby in the future we should optimize this, and/or provide mechanisms for outputting the score as a hierarchy
    
    OdotBundle bndl;
    
    long count = 0;
    string prefix = "/symbol/";
    for (auto it = score_symbols.begin(); it != score_symbols.end(); it++)
    {
        bndl.addMessage(prefix + to_string(count), *((*it).get()));
        count++;
    }
    
    return bndl.serialize();
}

/***********************************
 * Create Single JSON object string from Score Bundles
 ***********************************/
string Score::getJSON()
{
    string JSON = "{";
    bool addcomma = false;
    long count = 0;
    string addr = "\"/symbol/";
    for (auto it = score_symbols.begin(); it != score_symbols.end(); it++)
    {
        if( addcomma )
            JSON += ",";
        
        JSON += addr + to_string(count) + "\" : " + (*it)->getJSON();
        addcomma = true;
        
        count++;
    }
    JSON += "}";
    
    return JSON;
}


/***********************************
 * Get active symbols at time
 ***********************************/
OdotBundle_s Score::getSymbolsAtTime( float t )
{
    return time_points.getSymbolsAtTime( t );
}

void Score::removeSymbolTimePoints( Symbol* s )
{
    time_points.removeStaffAndSymbolTimePoints( s );
}

void Score::addSymbolTimePoints( Symbol* s )
{
    time_points.addSymbolTimePoints( s );
}


OdotBundle_s Score::getDurationBundle()
{
    auto lastTimePoint = time_points.getLastTimePoint();
    if( !lastTimePoint )
        return NULL;
    
    OdotBundle bndl;
    bndl.addMessage("/time/duration", lastTimePoint->time);
    
    return bndl.serialize();
}

/***********************************
 * Get the Nth Symbol of the Score
 ***********************************/
Symbol* Score::getSymbol(int n)
{
    if (n < score_symbols.size())
        return score_symbols[n].get();
    else
        throw length_error("Index " + to_string(n) + " is out of bound (score size is " + to_string(getSize()) + ").");
    
}

/***********************************
 * Get the number of symbols
 ***********************************/
size_t Score::getSize()
{
    return score_symbols.size();
}

/***********************************
 * Returns the position of a Symbol in the Score
 ***********************************/
int Score::getSymbolPosition(Symbol* s)
{
    auto iteratorToSymbol = find_if(score_symbols.begin(),
                                    score_symbols.end(),
                                    [s](unique_ptr<Symbol>& symbolFromScore) {
                                        return symbolFromScore.get() == s;
                                    });
    return static_cast<int>(distance(score_symbols.begin(), iteratorToSymbol));
}

const Array<Symbol* > Score::getSymbolsByValue(const string& address, const string& value)
{
    Array<Symbol* > matched;
    for (auto it = score_symbols.begin(); it != score_symbols.end(); it++)
    {
        OdotMessage val = (*it)->getMessage( address );
        
        if( val[0].getType() == OdotAtom::O_ATOM_STRING && val[0].getString() == value )
        {
            matched.add((*it).get());
        }
    }
    return matched;
}


const Symbol* Score::getStaveByID( const string& id )
{
    return staves.getStaveByID( id );
}

/***********************************
 * Add symbols from bundle, updating values if already in the score
 ***********************************/

void Score::importSymbols( const OdotBundle_s& s_bundle )
{
    OdotBundle bundle( s_bundle ); //<< deserializes the bundle
    
    DEBUG_FULL("===ITERATE OSC (" << bundle.size() << " messages)" << endl);
    for ( auto msg : bundle.getMessageArray() )
    {
        if( msg.getAddress().find("/symbol") == 0 && msg[0].getType() == OdotAtom::O_ATOM_BUNDLE )
        {
            Symbol* s = new Symbol( msg.getBundle().get_o_ptr() );
            addSymbol( s );
        }
    }
    DEBUG_FULL("===ITERATE DONE" << endl);
}

/***********************************
 * Clear current score and add symbols from bundle
 ***********************************/

void Score::importReplaceScore( const OdotBundle_s& s_bundle )
{
    removeAllSymbols();
    importSymbols( s_bundle );
}


/***********************************
 * Staff/Stave handling
 ***********************************/

void Score::addStaff(Symbol* s)
{
    staves.addStaff(s);
}

/***********************************
 * called when staff is moved
 ***********************************/

void Score::updateStaves(Symbol* moved_stave)
{

    string type = moved_stave->getMessage("/type").getString();
    
    if( type != "staff" )
        return;
    
    // 1. remove time points for moved stave
    // 2. resort the staves
    // 3. add time points for sybols on the stave
    
    // remove (this is done already in the modifySymbolInScore symbolist handler function)
    // time_points.removeStaffAndSymbolTimePoints(moved_stave);
    
    // sort staves and set times for each stave
    staves.resetTimes();
    
    // add the symbols
    
    String staff_id = moved_stave->getID();
    for(auto it = score_symbols.begin(); it != score_symbols.end(); it++)
    {
        if ((*it)->getSaff() == staff_id)
        {
            time_points.addSymbolTimePoints((*it).get());
        }
    }
    
    // time_points.resetTimes();
}

/*
 *  called when a stave is moved to update all timepoints, could be optimized in the future
 */

void Score::updateStavesAndTimepoints()
{
    
    // sort staves and set times for each stave
    staves.resetTimes();
    time_points.reset();
    
    // add the symbols
    
    for( auto it = score_symbols.begin(); it != score_symbols.end(); it++)
        time_points.addSymbolTimePoints((*it).get());
    
}

Symbol* Score::getStaveAtTime( float time )
{
    return staves.getStaveAtTime(time);
}


const StringArray Score::getStaves()
{
    return staves.getStaveNames();
}


/*
void Score::deleteOdotBundleArray(odot_bundle** bundle_array, int size)
{
    for (int i = 0; i < size; i++) {
        cout << "delete " << i << " " << bundle_array[i] << endl;
        delete bundle_array[i] ;
    }
    delete bundle_array ;
}
*/

