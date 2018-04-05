#include "Symbol.h"

void Symbol::setTypeXYWH(const string & type, float x, float y, float w, float h)
{
    
    addMessage( "/type", type );
    addMessage( "/x", x );
    addMessage( "/y", y );
    addMessage( "/w", w );
    addMessage( "/h", h );
    
//    print();
    
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
        std::cout << "failed to parse symbol:\t" << address << std::endl;
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

    cout << "setting " << pixelsToTime(start_x) << " " << pixelsToTime(dur_x) << endl;
    
    addMessage("/time/start",    pixelsToTime(start_x) );
    addMessage("/time/duration", pixelsToTime(dur_x) );

}

void Symbol::setTimeAndDuration( const float start_t, const float dur_t )
{
    addMessage("/time/start",   start_t );
    addMessage("/time/duration", dur_t );
    
}




