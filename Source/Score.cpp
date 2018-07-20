#include "Score.h"

#include <iostream>
#include <algorithm>
#include <vector>

Score::Score() : m_time_points(m_score), m_symbol_table(m_score), m_type_selector(m_score)
{
    // Sets the score_ptr reference for time_points instance variable.
    setDefaults();
}

Score::Score(const Score& src) : m_time_points(m_score), m_symbol_table(m_score), m_type_selector(m_score)
{
    // copy score
    m_score = src.m_score;
    setDefaults();
    
    m_symbol_table.select("/symbol");
    buildTimeLookups();

}

Score::Score( const OdotBundle_s& s_bundle  ) : m_time_points(m_score), m_symbol_table(m_score), m_type_selector(m_score)
{
    importSymbols( s_bundle );
    setDefaults();
    
    m_symbol_table.select("/symbol");
    buildTimeLookups();
}

Score::~Score()
{

}

/**
 *  Sets default stave sequencing functions.
 *  these functions should be moved to the stave or clef prototype
 *
 *  /stave/sort/fn      is a comparator
 *
 *  /stave/pixTime/fn   takes an argument t for the current time, and must set the symbol's start and end time.
 *                      the /time/end value is used for the text intput t
 *
 *  /stave/event/timePix/fn     takes and argument stave, and calculates /x and /w based on the symbol's
 *                               /time/start and /time/end values (required).
 */
void Score::setDefaults()
{
    if( !m_score.addressExists("/stave/sort/fn") )
        m_score.addMessage("/stave/sort/fn",
                           R"(
                                lambda([a,b],
                                       (a./y < b./y) && (b./x < (a./x + a./w))
                                )
                           )" );
    
    if( !m_score.addressExists("/stave/pixTime/fn") )
    {
        m_score.addMessage("/stave/pixTime/fn",
                           R"(
                           lambda([t],
                                    /time/start = t,
                                    /time/end = t + (/w * 0.01)
                                  )
                           )" );
    }
    
    if( !m_score.addressExists("/stave/event/pixTime/fn") )
    {
        m_score.addMessage("/stave/event/pixTime/fn",
                           R"(
                           lambda([stave],
                                    /time/start = stave./time/start + (/x - stave./x) * 0.01 ,
                                    /time/end = /time/start + (/w * 0.01)
                                  )
                           )" );
    }
    
    if( !m_score.addressExists("/stave/event/timePix/fn") )
    {
        m_score.addMessage("/stave/event/timePix/fn",
                           R"(
                           lambda([stave],
                                    /x = stave./x + ( (/time/start - stave./time/start) * 100. ),
                                    /w = (/time/end - /time/start) * 100.
                                  )
                           )" );
    }
}

void Score::buildTimeLookups()
{
    /*
    1. connect Staves in time and set /time bundle in stave symbols
    1. find all staves and put them into order based on sorting rules
    2. set start and end times (also duration?) for each stave accumulating time from the previous staves
        note: this sorted vector doesn't really need to be saved if we are recalculating everything whenever a new symbol is added
    3. find all Symbols that are linked to staves and set the symbol /time bundle
    4. create TimePoint Array to optimize lookup into score sequence
    */
    
    m_type_selector.select( OdotMessage("/type", "staff") );
    auto stave_vec = m_type_selector.getVector();
    
    OdotMessage compareFn = m_score.getMessage("/stave/sort/fn");
    OdotExpr compareExpr( "/order = /stave/sort/fn( /stave/a, /stave/b )" );
    
    sort( stave_vec.begin(), stave_vec.end(),
         [&compareExpr, &compareFn](OdotMessage& a, OdotMessage& b){
             OdotBundle test(compareFn);
             test.addMessage("/stave/a", a.getBundle() );
             test.addMessage("/stave/b", b.getBundle() );
             test.applyExpr( compareExpr );
             return test.getMessage("/order").getInt();
         });
    
    
    OdotMessage pixTimeFn = m_score.getMessage("/stave/pixTime/fn");
    OdotExpr pixTimeApplyExpr(  R"(
                              /stave/pixTime/fn( /t ),
                              delete(/stave/pixTime/fn), delete(/t)
                              )" );
    
    float time = 0.0f;
    for( auto& staff_msg : stave_vec )
    {
        auto staff = staff_msg.getBundle();
        staff.addMessage("/t", time);
        staff.addMessage( pixTimeFn );
        staff.applyExpr( pixTimeApplyExpr );
        m_score.addMessage( staff_msg.getAddress(), staff );
        
        time = staff.getMessage("/time/end").getFloat();
        
        // addStaff was doing this naming business which should be somewhere else
        /*
         string name = staff.getMessage("/name").getString();
         if( name.empty() ) // For now allow  name == s->getID()
         staff.addMessage( "/name", "staff_" + to_string(staves.size()) );
         */
    }
    
    
    time_points.reset();

    OdotExpr eventPixTimeApplyExpr(  R"~(
                                   /stave/event/pixTime/fn( /stave ),
                                   delete(/stave/event/pixTime/fn), delete(/stave)
                                   )~" );
    
    auto symbol_vec = m_symbol_table.getVector();
    for( auto& sym_msg : symbol_vec )
    {
        auto sym = sym_msg.getBundle();
        auto staff_id = sym.getMessage("/staff/id").getString();
        if( staff_id.size() > 0 )
        {
            auto linked_staff = m_symbol_table[staff_id].getBundle();
            if( linked_staff.size() > 0 )
            {
                sym.addMessage("/stave", linked_staff );
                sym.addMessage( eventPixTimeFn );
                sym.applyExpr( eventPixTimeApplyExpr );
                m_score.addMessage(sym_msg.getAddress(), sym );
                
                m_time_points.addSymbol( sym, linked_staff );
            }
        }
    }
    
}


void Score::print() const
{
    m_score.print();
}

/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::removeAllSymbols()
{
    m_score.clear();
    time_points.reset();
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
    DEBUG_FULL(symbol << endl )

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
    DEBUG_FULL("checking for " << id << endl )

    if( id == "" )
        DEBUG_FULL("The symbol " << symbol << " has no id text." << endl);
    
    auto iteratorToSymbol = find_if(score_symbols.begin(),
                                    score_symbols.end(),
                                    [id]( unique_ptr<Symbol>& ptrToSymbol )
                                    {
                                        return (ptrToSymbol->getID() == id );
                                    });
    
    if (iteratorToSymbol != score_symbols.end())
    {
        (*iteratorToSymbol)->unionWith( symbol, true );
        DEBUG_FULL("iteratorToSymbol " << endl )
        return (*iteratorToSymbol).get();
    }
    
    // make copy and add to symbol vector
    score_symbols.push_back( unique_ptr<Symbol>( new Symbol(*symbol) ) );
    

    /* Retrieves the last inserted symbol's reference
     * before sorting the score
     */
    Symbol* lastInsertedSymbol = score_symbols.back().get();
    DEBUG_FULL("lastInsertedSymbol" << lastInsertedSymbol << endl )

    // Calls the sort function to properly insert the new symbol.
    // ... thinking: this means that the score is sorted in time, but actually the time aspect should be separate from the score symbol order, the symbol order should be the drawing order (the same as the component children order)
    sort(score_symbols.begin(), score_symbols.end(), score_sorter);
    
    /* lastInsertedSymbol is added to staves
     * only if it is of type staff.
     */
    DEBUG_FULL("attempt to add staff " << lastInsertedSymbol << endl )
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
            if ((*it)->getStaff() == lastInsertedSymbol->getID())
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
    string nextID = name + "/" + to_string(count++);
    
    while( find( ids.begin(), ids.end(), nextID ) != ids.end() )
    {
        nextID = name + "/" + to_string(count++);
    }
    
    symbol->addMessage("/id", nextID );
    //DEBUG_FULL(nextID << endl)
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
    OdotBundle bndl("/time/duration", time_points.getTotalDuration() );
    
    return bndl.serialize();
}

/***********************************
 * Get the Nth Symbol of the Score
 ***********************************/
Symbol* Score::getSymbol(int n)
{
    m_score.getMessage( "/symbol/" + to_string(n) );
    
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
int Score::getSymbolPosition(Symbol* symbol)
{
    auto iteratorToSymbol = find_if(score_symbols.begin(),
                                    score_symbols.end(),
                                    [symbol](unique_ptr<Symbol>& symbolFromScore) {
                                        return symbolFromScore.get() == symbol;
                                    });
	
	int symbolPosition = static_cast<int>(distance(score_symbols.begin(), iteratorToSymbol));
	
    return (symbolPosition < score_symbols.size()) ? symbolPosition : -1;
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
    OdotBundle bundle( s_bundle ); // Deserializes the bundle
	Symbol* newSymbol;
	
    DEBUG_FULL("===ITERATE OSC (" << bundle.size() << " messages)" << endl)
    for ( auto msg : bundle.getMessageArray() )
    {
    	/* If the message contains a bundle then creates a new symbol from this bundle
		 *
    	 */
        if( msg.getAddress().find("/symbol") == 0 && msg[0].getType() == OdotAtom::O_ATOM_BUNDLE )
        {
			DEBUG_FULL("Message type is bundle." << endl)
			newSymbol = new Symbol( msg.getBundle().get_o_ptr() ); // ideally don't use new here
			addSymbol( newSymbol );
        }
        /* If the message contains a string then tries to create a symbol from this string.
		 * invalid_argument exception is caught if the string, which normally describes a OSC bundle, is not
		 * well-formed.
         */
		else if ( msg.getAddress().find("/symbol") == 0 && msg[0].getType() == OdotAtom::O_ATOM_STRING )
		{
			DEBUG_FULL("Message type is string." << endl)
			try {
  				newSymbol = new Symbol( msg[0].getString() ); // ideally don't use new here
				addSymbol( newSymbol );
			} catch (invalid_argument& error) {
  				cout << error.what() << endl;
			}
	
		}
		else DEBUG_FULL("Message type is neither bundle nor string." << endl);
	
	}
	
    DEBUG_FULL("===ITERATE DONE" << endl)
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
/*
void Score::addStaff(Symbol* s)
{
    staves.addStaff(s);
}
*/

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
        if ((*it)->getStaff() == staff_id)
        {
            time_points.addSymbolTimePoints((*it).get());
        }
    }
    
    // time_points.resetTimes();
}

Symbol* Score::getStaveAtTime( float time )
{
    return staves.getStaveAtTime(time);
}

const StringArray Score::getStaves()
{
    return staves.getStaveNames();
}

int Score::getNameCount( string& name )
{
	int count = 0;
	for(auto iteratorToSymbol = score_symbols.begin(); iteratorToSymbol != score_symbols.end(); iteratorToSymbol++)
		if( (*iteratorToSymbol)->getName() == name )
			count++;
	
	return count;
}

bool Score::idExists( string& searchedId )
{
	bool idFound = false;
	auto iteratorToSymbol = score_symbols.begin();
	
	while (!idFound && iteratorToSymbol != score_symbols.end())
	{
		idFound = (*iteratorToSymbol)->idExists(searchedId);
		iteratorToSymbol++;
	}
	
	return idFound;
}

