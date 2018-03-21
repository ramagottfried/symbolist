
#pragma once
#include "../JuceLibraryCode/JuceHeader.h"
#include "types.h"
#include "OdotBundle.hpp"

using namespace std;

//============================
// SYMBOL
//============================

class Symbol : public OdotBundle
{
public:
    
    using OdotBundle::OdotBundle;
    
    void setTypeXYWH(const string & type, float x, float y, float w, float h);

    bool symbol_parse_error( int p, const string& address ) const;
    
    void printBundle() const;

    OdotBundle_s    exportToOSC();
    void            importFromOSC( OdotBundle_s& s_bundle );
    
    float getTime() const;
    float getDuration() const;
    float getEndTime() const;
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
        return pixelsToTime( getMessage("/x").getFloat() );
    }
    
    bool hitTestTime( float t )
    {
        float start = getTime();
        return t >= start && t <= ( start + getDuration() );
    }
    
private:
    string  m_address;
    
    float       m_pixels_to_time = 0.01f;
    float       m_time_to_pixels = 100.0f;

};

