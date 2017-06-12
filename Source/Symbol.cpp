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
    addOSCMessage(String("/type"), type);
    addOSCMessage(String("/x"), x);
    addOSCMessage(String("/y"), y);
    addOSCMessage(String("/w"), w);
    addOSCMessage(String("/h"), h);
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

void Symbol::printBundle()
{
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

