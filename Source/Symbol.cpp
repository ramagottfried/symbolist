#include "Symbol.h"

void Symbol::setTypeXYWH(const string & type, float x, float y, float w, float h)
{
    addMessage( "/type", type );
    addMessage( "/x", x );
    addMessage( "/y", y );
    addMessage( "/w", w );
    addMessage( "/h", h );
    
    // add name?
}

OdotBundle_s Symbol::exportToOSC()
{
    return serialize();
}

string Symbol::getID()
{
    return getMessage("/id").getString();
}

string Symbol::getName() const
{
    return getMessage("/name").getString();
}

string Symbol::getSaff()
{
    return getMessage("/staff").getString();
}

string Symbol::getType()
{
    return getMessage("/type").getString();
}

float Symbol::getTime() const
{
    return getMessage("/time/start").getFloat();
}

float Symbol::getDuration() const
{
    return getMessage("/time/duration").getFloat();
}

float Symbol::getEndTime() const
{
    return ( getTime() + getDuration() );
}

bool Symbol::symbol_parse_error( int p, const string& address ) const
{
    if( p == -1 )
    {
        DEBUG_FULL("Failed to parse symbol of address " << address << endl)
        return true; // there is an error
    }
    return false;
}

void Symbol::setPosition( const Point<float> pos )
{
    addMessage("/x", pos.getX() );
    addMessage("/y", pos.getY() );
}

void Symbol::setTimeAndDurationFromRelPix( const float start_x, const float dur_x )
{

    DEBUG_FULL("Setting " << pixelsToTime(start_x) << " " << pixelsToTime(dur_x) << endl)
    
    addMessage("/time/start",    pixelsToTime(start_x) );
    addMessage("/time/duration", pixelsToTime(dur_x) );

}

void Symbol::setTimeAndDuration( const float start_t, const float dur_t )
{
    addMessage("/time/start",   start_t );
    addMessage("/time/duration", dur_t );
    
}

bool Symbol::idExists(string& searchedId)
{
	bool idFound = (getID() == searchedId);
	
	/* If the current symbol's id doesn't match searchedId
	 * then, if it's a group, looks for the id in its inner symbols.
	 */
	if (!idFound && getType() == GROUP)
	{
		/* Returns the list of OdotMessage in this Symbol instance
		 * which address matches "/subsymbol"
		 */
    	vector<OdotMessage > subSymbolMessages = matchAddress( "/subsymbol", false );
		auto iteratorToMessage = subSymbolMessages.begin();
		OdotMessage subSymbolMessage;
		Symbol subSymbol;
		
		while (!idFound && iteratorToMessage != subSymbolMessages.end())
    	{
			subSymbolMessage = (*iteratorToMessage);
			
			/* Verifies that the first argument of
			 * the subsymbol message is a bundle.
			 */
			if( subSymbolMessage[0].isBundle() )
			{
				subSymbol = Symbol(subSymbolMessage.getBundle().get_o_ptr());
				idFound = subSymbol.idExists(searchedId);
			}
			
			iteratorToMessage++;
    	}
	}
	
	return idFound;
}

void Symbol::resetAllIds()
{
	addMessage("/id", "");
	
	/* If the current symbol is a group,
	 * resets its inner symbols' id.
	 */
	if (getType() == "group" || getType() == "staff")
	{
		/* Returns the list of OdotMessage in this Symbol instance
		 * matching the address "/subsymbol"
		 */
    	vector<OdotMessage > subSymbolMessages = matchAddress( "/subsymbol", false );
		Symbol subSymbol;
		
		for (OdotMessage subSymbolMessage : subSymbolMessages)
			/* Verifies that the first argument of
			 * the subsymbol message is a bundle.
			 */
			if( subSymbolMessage[0].isBundle() )
			{
				subSymbol = Symbol(subSymbolMessage.getBundle().get_o_ptr());
				subSymbol.resetAllIds();
				
				addMessage(subSymbolMessage.getAddress(), subSymbol);
				
			}
	}
	
}

