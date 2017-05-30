//
//  Symbol.cpp
//  symbolist
//
//  Created by Jean Bresson on 30/05/2017.
//
//

#include "Symbol.h"


Symbol::Symbol() : Symbol( OSCBundle() ) {};

Symbol::Symbol(OSCBundle b)
{
    osc_bundle = b;
}

int Symbol::getOSCMessagePos(const char* address)
{
    String addr(address);
    for (int i = 0; (i < osc_bundle.size()) ; i++)
    {
        if ( osc_bundle.operator[](i).getMessage().getAddressPattern().toString().equalsIgnoreCase(addr) )
        {
            return i;
        }
    }
    return -1;
}

OSCArgument Symbol::getOSCMessageValue(int pos)
{
    OSCBundle::Element e = osc_bundle.operator[](pos);
    return e.getMessage().operator[](0);
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

