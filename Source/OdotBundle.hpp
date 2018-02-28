#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "osc_bundle_u.h"
#include "osc_bundle_s.h"
#include "osc_strfmt.h"

using namespace std;

class OdotBundle
{
    
public:
    OdotBundle();
    OdotBundle( const OdotBundle& src );
    OdotBundle( t_osc_bndl_u *src );
    
    OdotBundle& operator= ( const OdotBundle& src );
    
    
    ~OdotBundle();
    
    void clear();
    
    void addOSCMessage( const String &address, const float value );
    void addOSCMessage( const String &address, const int value );
    void addOSCMessage( const String &address, const String &value );
    
    void addSubbundle( const String &address, const OdotBundle& subbundle );
    
    bool addressExists( const String& address );
    t_osc_msg_ar_u *lookupAddress( const String &address);
    
    String oscAddressGetString(const String& address);
    float oscAddressGetFloat(const String& address);
    int oscAddressGetInt(const String& address);
    ScopedPointer<OdotBundle> oscAddressGetBundle(const String& address);
    
    void print() const;
    
private:
    
    void delete_bundle();
    
    t_osc_bndl_u *bundle;
    
    JUCE_LEAK_DETECTOR (OdotBundle)
  
};


typedef struct _osc_atom_u{
    union _word{
        int8_t c;
        uint8_t C;
        int16_t u;
        uint16_t U;
        int32_t i;
        uint32_t I;
        int64_t h;
        uint64_t H;
        float f;
        double d;
        float q[4];
        double Q[4];
        char *s;
        t_osc_bndl_u *bndl;
        t_osc_timetag t;
        char *b;
        void *rec;
    } w;
    int typetag;
    int alloc;
    struct _osc_atom_u *next, *prev;
} t_osc_atom_u;
