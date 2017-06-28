

#include "Symbol.h"

Symbol::Symbol () {};

Symbol::Symbol (const String & type, float x, float y, float w, float h)
{
    addOSCMessage("/type",  type);
    addOSCMessage("/x",     x);
    addOSCMessage("/y",     y);
    addOSCMessage("/w",     w);
    addOSCMessage("/h",     h);
    
    addOSCMessage("/time/start",        pixelsToTime( x ) );
    addOSCMessage("/time/duration",     pixelsToTime( w ) );
}


String Symbol::getType()
{
    return getOSCMessageValue("/type").getString();
}

float Symbol::getTime() const
{
    return getOSCMessageValue("/time/start").getFloat32();
}

float Symbol::getDuration() const
{
    return getOSCMessageValue("/time/duration").getFloat32();
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


// filter the symbol from base_address
Symbol Symbol::makeSubSymbol( const String &base_address ) const
{
    Symbol s;
    
    for (int i = 0; (i < osc_bundle.size()) ; i++)
    {
        String addr = osc_bundle[i].getMessage().getAddressPattern().toString() ;
        
        if ( addr.startsWith( base_address ) && addr[base_address.length()] == '/' )
        {
            OSCMessage m (OSCAddressPattern(addr.substring(base_address.length())));

            for (int mi = 0; mi < osc_bundle[i].getMessage().size(); mi++)
            {
                m.addArgument( osc_bundle[i].getMessage()[mi] );
            }
            s.addOSCMessage(m);
        }
    }
    return s;
}

void Symbol::setPosition( const Point<float> pos )
{
    cout << this << " set x position " << pos.getX() << endl;
    
    OSCBundle new_bundle;

    // there must be a better way to do this!
    for (auto osc : osc_bundle )
    {
        if( !osc.getMessage().getAddressPattern().toString().equalsIgnoreCase( "/x" ) &&
            !osc.getMessage().getAddressPattern().toString().equalsIgnoreCase( "/y" ) &&
            !osc.getMessage().getAddressPattern().toString().equalsIgnoreCase( "/time/start" ) &&
            !osc.getMessage().getAddressPattern().toString().equalsIgnoreCase( "/time/duration" ) )
        {
            new_bundle.addElement(osc);
        }
    }
    
    osc_bundle = new_bundle;
    
    addOSCMessage(String("/x"), pos.getX() );
    addOSCMessage(String("/y"), pos.getY() );
    
    addOSCMessage(String("/time/start"), pos.getX() * 0.01f );
    addOSCMessage(String("/time/duration"), getOSCMessageValue("/w").getFloat32() * 0.01f );

    //printBundle();
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

void Symbol::addOSCMessage( const String &address )
{
    osc_bundle.addElement(OSCBundle::Element(OSCMessage(OSCAddressPattern(address))));
}

void Symbol::addOSCMessage( const OSCMessage m )
{
    osc_bundle.addElement(m);
}

void Symbol::addOSCMessage( const String &address, const float value)
{
    osc_bundle.addElement(OSCBundle::Element(OSCMessage(OSCAddressPattern(address), value)));
}

void Symbol::addOSCMessage( const String &address, const int value)
{
    osc_bundle.addElement(OSCBundle::Element(OSCMessage(OSCAddressPattern(address), value)));
}

void Symbol::addOSCMessage( const String &address, const String &value)
{
    osc_bundle.addElement(OSCBundle::Element(OSCMessage(OSCAddressPattern(address), value)));
}

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

