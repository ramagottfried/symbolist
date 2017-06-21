//
//  Symbol.cpp
//  symbolist
//
//  Created by Jean Bresson on 30/05/2017.
//
//

#include "Symbol.h"

//: Symbol ("undefined", 0.0, 0.0, 10.0, 10.0)
Symbol::Symbol () {};

Symbol::Symbol (const String & type, float x, float y, float w, float h)
{
    addOSCMessage("/type", type);
    addOSCMessage("/x", x);
    addOSCMessage("/y", y);
    addOSCMessage("/w", w);
    addOSCMessage("/h", h);
    
    addOSCMessage("/offset", x * 10.0f);
    addOSCMessage("/duration", 500.0f);
    
}

String Symbol::getType()
{
    return getOSCMessageValue("/type").getString();
}

int Symbol::getTime() const
{
    return (int)getOSCMessageValue("/offset").getFloat32();
}

int Symbol::getDuration() const
{
    return (int)getOSCMessageValue("/duration").getFloat32();
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
    
    OSCBundle new_bundle;
    
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

float Symbol::getOSCValueAsFloat(OSCArgument a)
{
    if( a.isFloat32() )
        return a.getFloat32();
    else if( a.isInt32() )
        return (float)a.getInt32();
    else
        return 0.0f;
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
    std::cout << "==== OSC BUNDLE ====" << std::endl;
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

