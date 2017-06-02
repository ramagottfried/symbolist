//
//  Symbol.hpp
//  symbolist
//
//  Created by Jean Bresson on 30/05/2017.
//
//

#ifndef Symbol_h
#define Symbol_h

#include "../JuceLibraryCode/JuceHeader.h"
#include "OSCIO.h"
#include "types.h"

//============================
// SYMBOL
//============================

class Symbol {
    
public:
    
    Symbol() = default;
    Symbol(const Symbol& other) = default;
    ~Symbol() = default;
    
    OSCBundle   getOSCBundle () { return osc_bundle; }
    void        setOSCBundle (OSCBundle *b) { osc_bundle = *b; }
    void        clearOSCBundle () { osc_bundle = OSCBundle(); }
    
    int         getOSCMessagePos(const String &address);
    OSCArgument getOSCMessageValue(const int pos);
    OSCArgument getOSCMessageValue(const String &address);
    void addOSCMessage( const String &address );
    void addOSCMessage( const OSCMessage m );
    void addOSCMessage( const String &address, const float value );
    void addOSCMessage( const String &address, const int value );
    void addOSCMessage( const String &address, const String &value );
    
    odot_bundle* exportToOSC();
    void        importFromOSC(odot_bundle *bundle);
    
private:
    
    OSCBundle   osc_bundle;
};



#endif /* Symbol_hpp */
