
#pragma once
#include "../JuceLibraryCode/JuceHeader.h"
#include "OSCIO.h"
#include "types.h"
#include "OdotBundle.hpp"

using namespace std;

//============================
// SYMBOL
//============================

class Symbol
{
    
public:
    
    Symbol();
    Symbol(const string &type, float x, float y, float w, float h );
    Symbol(const Symbol& other);
    Symbol( const OdotBundle& src );
    void setBundle( OdotBundle& src);
    
    Symbol& operator= ( const Symbol& src );
    
    // move
    Symbol( Symbol&& src ) = default;
    Symbol& operator=( Symbol&& src ) = default;
    
    ~Symbol();
    
    OdotMessage getMessage( const string &address ) const
    {
        return o_bundle.getMessage( address );
    }
    
    const ScopedPointer<Symbol>
    getSubSymbol( const string &base_address );
    
    void addSubSymbol( const string &address, const Symbol& symbol );

    template<typename... Ts>
    void addMessage(const string& address, Ts... args)
    {
        o_bundle.addMessage( address, args... );
    }
    
    bool addressExists( const string& address )
    {
        return o_bundle.addressExists( address );
    }
    
    const OdotBundle *  getBundle () const {
        return &o_bundle;
    }
    
    void setBundle (OdotBundle *b)
    {
        o_bundle = *b;
    }
    
    void clearBundle () {
        o_bundle.clear();
    }
    
    bool symbol_parse_error( int p, const string& address ) const;
    
    void printBundle() const;

    
    OdotBundle_s    exportToOSC();
    void            importFromOSC( OdotBundle_s& s_bundle );
    
    Symbol makeSubSymbol( const string &base_address ) const;
    
    
    float getTime();
    float getDuration();
    float getEndTime();
    string getName();
    string getID();
    string getSaff();
    string getType();

    
    void setPosition( const Point<float> pos );
    void setID( const string& str );
    void setName( const string& str );


    void setTimeAndDurationFromRelPix( const float start_x, const float dur_x );
    void setTimeAndDuration( const float start_t, const float dur_t );

    
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
        return pixelsToTime( o_bundle.getMessage("/x").getFloat() );
    }

    
    // unused?
    bool hitTest( float t )
    {
        return (t >= getTime() && t <= getEndTime());
    }

    
private:
    
    OdotBundle      o_bundle;
        
    float       m_pixels_to_time = 0.01f;
    float       m_time_to_pixels = 100.0f;
    
    Path        symbol_path;
    
};

