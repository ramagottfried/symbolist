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
    
    Symbol();
    Symbol(OSCBundle b);
    
    OSCBundle   getOSCBundle () { return osc_bundle; }
    void        setOSCBundle (OSCBundle b) { osc_bundle = b; }
    
    int getOSCMessagePos(const char* address);
    OSCArgument getOSCMessageValue(int pos);
    odot_bundle* exportToOSC();
    void importFromOSC(odot_bundle *bundle);
    
private:
    
    OSCBundle osc_bundle;
    t_rect m_rect;
};



#endif /* Symbol_hpp */
