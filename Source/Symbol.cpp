

#include "Symbol.h"


Symbol::Symbol () {}

Symbol::Symbol(const Symbol& other)
{
    o_bundle = other.o_bundle;
    
    osc_bundle = other.osc_bundle;
}

Symbol::Symbol(const OdotBundle& bundle)
{
    o_bundle = bundle;
}

Symbol::Symbol(const String & type, float x, float y, float w, float h)
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

void Symbol::setBundle( OdotBundle& src)
{
    o_bundle = src;
}

Symbol::~Symbol() {}

void Symbol::setID( const String& str )
{
    o_bundle.addMessage( "/id", str );
}

String Symbol::getID()
{
    return o_bundle.getMessage("/id").getString();
}

String Symbol::getName()
{
    return o_bundle.getMessage("/name").getString();
}

void Symbol::setOSCAddrAndValue( const String& addr, const String& value  )
{
    int pos = getOSCMessagePos(addr);
    if( pos == -1 )
        addOSCMessage(addr, value );
    else
    {
        OSCBundle newBundle;
        for( auto osc_m_iter : osc_bundle )
        {
            auto i_msg = osc_m_iter.getMessage();
            if( i_msg.getAddressPattern().toString() == addr )
                newBundle.addElement(OSCBundle::Element(OSCMessage(OSCAddressPattern(addr), value)));
            else
                newBundle.addElement( i_msg );
        }
        osc_bundle = newBundle;
        
    }
}

void Symbol::setOSCAddrAndValue( const String& addr, const float value  )
{
    int pos = getOSCMessagePos(addr);
    if( pos == -1 )
        addOSCMessage(addr, value );
    else
    {
        OSCBundle newBundle;
        for( auto osc_m_iter : osc_bundle )
        {
            auto i_msg = osc_m_iter.getMessage();
            if( i_msg.getAddressPattern().toString() == addr )
                newBundle.addElement(OSCBundle::Element(OSCMessage(OSCAddressPattern(addr), value)));
            else
                newBundle.addElement( i_msg );
        }
        osc_bundle = newBundle;
        
    }
}


String Symbol::getSaff()
{
    int pos = getOSCMessagePos("/staff");
    if( pos != -1 )
    {
        return getOSCMessageValue(pos).getString();
    }
    
    return String();

}


String Symbol::getType()
{
    return getOSCMessageValue("/type").getString();
}

float Symbol::getTime() const
{
    int pos = getOSCMessagePos("/time/start");
    if( pos == -1 )
        return -1;
    else
        return getOSCMessageValue(pos).getFloat32();
}

float Symbol::getDuration() const
{
    int pos = getOSCMessagePos("/time/duration");
    if( pos == -1 )
        return -1;
    else

    return getOSCMessageValue(pos).getFloat32();
}

float Symbol::getEndTime() const
{
    return ( getTime() + getDuration() );
}


bool Symbol::symbol_parse_error( int p, const String& address ) const
{
    if( p == -1 )
    {
        std::cout << "failed to parse symbol:\t" << address << std::endl;
        return true; // there is an error
    }
    return false;
}

const ScopedPointer<Symbol> Symbol::getSubSymbol( const String &base_address )
{
    OdotBundle b = o_bundle.getMessage( base_address.getCharPointer() ).getBundle();
    return ScopedPointer<Symbol>( new Symbol( b ) );
}

void Symbol::addSubSymbol( const String &base_address, const Symbol& symbol )
{
    OdotBundle bndl_cpy( symbol.o_bundle );
    o_bundle.addMessage( base_address.getCharPointer(), bndl_cpy );
}


void Symbol::setPosition( const Point<float> pos )
{
    cout << this << " set x position " << pos.getX() << endl;
    
    OSCBundle new_bundle;

    // there must be a better way to do this!
    for (auto osc : osc_bundle )
    {
        if( !osc.getMessage().getAddressPattern().toString().equalsIgnoreCase( "/x" ) &&
            !osc.getMessage().getAddressPattern().toString().equalsIgnoreCase( "/y" ) )
        {
            new_bundle.addElement(osc);
        }
    }
    
    osc_bundle = new_bundle;
    
    addOSCMessage(String("/x"), pos.getX() );
    addOSCMessage(String("/y"), pos.getY() );

    //printBundle();
}

void Symbol::setTimeAndDurationFromRelPix( const float start_x, const float dur_x )
{
    OSCBundle new_bundle;
    
    // there must be a better way to do this!
    for (auto osc : osc_bundle )
    {
        if(!osc.getMessage().getAddressPattern().toString().equalsIgnoreCase( "/time/start" ) &&
           !osc.getMessage().getAddressPattern().toString().equalsIgnoreCase( "/time/duration" ) )
        {
            new_bundle.addElement(osc);
        }
    }
    
    osc_bundle = new_bundle;
    
    cout << "setting " << pixelsToTime(start_x) << " " << pixelsToTime(dur_x) << endl;
    
    addOSCMessage(String("/time/start"),    pixelsToTime(start_x) );
    addOSCMessage(String("/time/duration"), pixelsToTime(dur_x) );

}

void Symbol::setTimeAndDuration( const float start_t, const float dur_t )
{
    OSCBundle new_bundle;
    
    // there must be a better way to do this!
    for (auto osc : osc_bundle )
    {
        if(!osc.getMessage().getAddressPattern().toString().equalsIgnoreCase( "/time/start" ) &&
           !osc.getMessage().getAddressPattern().toString().equalsIgnoreCase( "/time/duration" ) )
        {
            new_bundle.addElement(osc);
        }
    }
    
    osc_bundle = new_bundle;
    
    cout << "setting time " << pixelsToTime(start_t) << " " << pixelsToTime(dur_t) << endl;
    
    addOSCMessage(String("/time/start"),   start_t );
    addOSCMessage(String("/time/duration"), dur_t );
    
}


int Symbol::getOSCMessagePos(const String &address) const
{
    for (int i = 0; (i < osc_bundle.size()) ; i++)
    {
        if ( osc_bundle[i].getMessage().getAddressPattern().toString().equalsIgnoreCase( address ) )
        {
            return i;
        }
    }
    return -1;
}

OSCArgument Symbol::getOSCMessageValue(const int pos) const
{
    OSCBundle::Element e = osc_bundle[pos];
    return e.getMessage()[0];
}

OSCArgument Symbol::getOSCMessageValue(const String &address) const
{
    int pos = getOSCMessagePos(address);
    
    if (pos >= 0) {
    
        return getOSCMessageValue(pos);
    
    } else {
        std::cout << "Error could not find OSC message with address = " << address << std::endl;
        return OSCArgument(-1); // should raise or print an error
    }
}

float Symbol::getOSCValueAsFloat(const OSCArgument& a)
{
    if( a.isFloat32() )
        return a.getFloat32();
    else if( a.isInt32() )
        return (float)a.getInt32();
    else
        return 0.0f;
}

int Symbol::getOSCValueAsInt(const OSCArgument& a)
{
    if( a.isInt32() )
        return a.getInt32();
    else if( a.isFloat32() )
        return (int)a.isFloat32();
    else
        return 0;
}

/*
void Symbol::addOSCMessage( const String &address )
{
    osc_bundle.addElement(OSCBundle::Element(OSCMessage(OSCAddressPattern(address))));
}

void Symbol::addOSCMessage( const OSCMessage m )
{
    osc_bundle.addElement(m);
}
*/

/*
void Symbol::addOSCMessage( const String &address, const float value)
{
    o_bundle.addOSCMessage(address, value);
    osc_bundle.addElement(OSCBundle::Element(OSCMessage(OSCAddressPattern(address), value)));
}

void Symbol::addOSCMessage( const String &address, const int value)
{
    o_bundle.addOSCMessage(address, value);
    osc_bundle.addElement(OSCBundle::Element(OSCMessage(OSCAddressPattern(address), value)));
}

void Symbol::addOSCMessage( const String &address, const String &value)
{
    o_bundle.addOSCMessage(address, value);
    osc_bundle.addElement(OSCBundle::Element(OSCMessage(OSCAddressPattern(address), value)));
}
*/

void Symbol::printBundle() const
{
    std::cout << "==== " << this << " OSC BUNDLE ====" << std::endl;
    for (auto osc : osc_bundle )
    {
        OSCMessage msg = osc.getMessage();
        std::cout << msg.getAddressPattern().toString();
        
        for (auto arg : msg )
        {
            if( arg.isString() )
                std::cout << " " << arg.getString();
            else if( arg.isFloat32() )
                std::cout << " " << (String)arg.getFloat32();
            else if( arg.isInt32() )
                std::cout << " " << (String)arg.getInt32();
            else if( arg.isBlob() )
                std::cout << " " << "blob";
        }
        
        std::cout << std::endl;
    }
    std::cout << "====-===-======-====" << std::endl;

}

odot_bundle* Symbol::exportToOSC()
{
    OSCWriter w ;
    w.writeBundle( osc_bundle );
    //cout << "BUNDLE WRITING: " << write_result << endl;
    size_t size = w.getDataSize();
    odot_bundle *bundle = new odot_bundle;
    bundle->len = static_cast<long>(size);
    bundle->data = new char[size];
    std::memcpy(bundle->data, w.getData() ,size );
    
    //std::cout << "encoded " << bundle->len << " bytes : " << bundle->data << std::endl;
    //for (int c = 0; c < bundle->len; c++) { cout << bundle->data[c] << "|"; }
    //cout << endl;
    
    return bundle;
}

void Symbol::importFromOSC(odot_bundle *bundle)
{
    OSCReader r ( bundle->data, bundle->len );
    //std::cout << "decoding " << bundle->len << " bytes : " << bundle->data << std::endl;
    //for (int c = 0; c < bundle->len; c++) { cout << bundle->data[c] << "|"; }
    //cout << endl;
    osc_bundle = r.readBundle();
}

