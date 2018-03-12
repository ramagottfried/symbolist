

#include "Symbol.h"


Symbol::Symbol () {}

Symbol::Symbol(const Symbol& other)
{
    o_bundle = other.o_bundle;
}

Symbol::Symbol(const OdotBundle& bundle)
{
    o_bundle = bundle;
}

Symbol::Symbol(const string & type, float x, float y, float w, float h)
{
    o_bundle.clear();

    o_bundle.addMessage( "/type", type );
    o_bundle.addMessage( "/x", x );
    o_bundle.addMessage( "/y", y );
    o_bundle.addMessage( "/w", w );
    o_bundle.addMessage( "/h", h );
    
    o_bundle.print();
    
    // add name?
}


Symbol& Symbol::operator=( const Symbol& src )
{
    //    D_(cout << __func__  << "copy= \n";)
    
    if( this != &src )
    {
        o_bundle = src.o_bundle;
    }
    
    return *this;
}


void Symbol::setBundle( OdotBundle& src)
{
    o_bundle = src;
}

Symbol::~Symbol() {}

void Symbol::setID( const string& str )
{
    o_bundle.addMessage( "/id", str );
}


string Symbol::getID()
{
    return o_bundle.getMessage("/id").getString();
}

string Symbol::getName()
{
    return o_bundle.getMessage("/name").getString();
}


string Symbol::getSaff()
{
    return o_bundle.getMessage("/staff").getString();
}


string Symbol::getType()
{
    return o_bundle.getMessage("/staff").getString();
}

float Symbol::getTime()
{
    return o_bundle.getMessage("/time/start").getFloat();
}

float Symbol::getDuration()
{
    return o_bundle.getMessage("/time/duration").getFloat();
}

float Symbol::getEndTime()
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

const ScopedPointer<Symbol> Symbol::getSubSymbol( const string &base_address )
{
    OdotBundle b = o_bundle.getMessage( base_address ).getBundle();
    return ScopedPointer<Symbol>( new Symbol( b ) );
}

void Symbol::addSubSymbol( const string &address, const Symbol& symbol )
{
    OdotBundle bndl_cpy( symbol.o_bundle );
    o_bundle.addMessage( address, bndl_cpy );
}


void Symbol::setPosition( const Point<float> pos )
{
    cout << this << " set x position " << pos.getX() << endl;
    
    addMessage("/x", pos.getX() );
    addMessage("/y", pos.getY() );

    //printBundle();
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

void Symbol::printBundle() const
{
    o_bundle.print();
}

OdotBundle_s Symbol::exportToOSC()
{
    
    return o_bundle.serialize();
}

void Symbol::importFromOSC( OdotBundle_s& s_bundle )
{
    o_bundle = s_bundle.deserialize();
}

