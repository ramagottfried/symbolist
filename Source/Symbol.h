
#pragma once
#include "../JuceLibraryCode/JuceHeader.h"
#include "OSCIO.h"
#include "types.h"

using namespace std;

//============================
// SYMBOL
//============================

class Symbol
{
    
public:
    
    Symbol();
    Symbol(const String &type, float x, float y, float w, float h );
    Symbol(const Symbol& other) = default;
    ~Symbol()
    {
//        std::cout << "deleting symbol " << this << std::endl;
    }
    
    String      getType();
    
    const OSCBundle *getOSCBundle () const { return &osc_bundle; }
    void        setOSCBundle (OSCBundle *b) { osc_bundle = *b; }
    void        clearOSCBundle () { osc_bundle = OSCBundle(); }
    
    int         getOSCMessagePos(const String &address) const;
    OSCArgument getOSCMessageValue(const int pos) const;
    OSCArgument getOSCMessageValue(const String &address) const;
    
    static float getOSCValueAsFloat(const OSCArgument& a) ;
    static int getOSCValueAsInt(const OSCArgument& a) ;
    
    
    bool symbol_parse_error( int p, const String& address ) const;
    
    void printBundle() const;
    
    void addOSCMessage( const String &address );
    void addOSCMessage( const OSCMessage m );
    void addOSCMessage( const String &address, const float value );
    void addOSCMessage( const String &address, const int value );
    void addOSCMessage( const String &address, const String &value );
    
    odot_bundle*    exportToOSC();
    void            importFromOSC(odot_bundle *bundle);
    
    Symbol makeSubSymbol( const String &base_address ) const;
    
    float getTime() const ;
    float getDuration() const ;
    float getEndTime() const ;
    String getName() const;
    
    bool hitTest( float t )
    {
        return (t >= getTime() && t <= getEndTime());
    }
    
    void setPosition( const Point<float> pos );
    void setID( const String& str );
    void setName( const String& str );

    String getID();
    String getSaff();
    
    void setTimeAndDurationFromRelPix( const float start_x, const float dur_x );
    void setTimeAndDuration( const float start_t, const float dur_t );

    void setOSCAddrAndValue( const String& addr, const String& value  );
    void setOSCAddrAndValue( const String& addr, const float value  );
    
    inline float pixelsToTime( const float f ) const
    {
        return f * m_pixels_to_time;
    }
    
    inline float timeToPixels( const float t ) const
    {
        return t * m_time_to_pixels;
    }
    
    float calcTime() // << this should be dynamically settable somehow
    {
        return pixelsToTime( getOSCValueAsFloat( getOSCMessageValue("/x") ) );
    }
    
    
private:
    
    OSCBundle   osc_bundle;
    
    float       m_pixels_to_time = 0.01f;
    float       m_time_to_pixels = 100.0f;
    
    Path        symbol_path;
    
};

