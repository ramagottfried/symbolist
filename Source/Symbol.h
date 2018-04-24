#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "types.h"
#include "OdotBundle.hpp"
#include "symbolist-utils.hpp"

using namespace std;


typedef enum {
    PATH,
    CIRCLE,
    TRIANGLE,
    RECTANGLE,
    STAFF,
    GROUP,
    UNKNOWN
} t_sym_type;



//============================
// SYMBOL
//============================

class Symbol : public OdotBundle
{
public:
    
    using OdotBundle::OdotBundle;
    
    void setTypeXYWH(const string & type, float x, float y, float w, float h);

    bool symbol_parse_error( int p, const string& address ) const;

    OdotBundle_s exportToOSC();
    void         importFromOSC( OdotBundle_s& s_bundle );
    
    float getTime() const;
    float getDuration() const;
    float getEndTime() const;
    string getName() const;
    string getID();
    string getSaff();
    t_sym_type getType();
    
    void setPosition( const Point<float> pos );

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
    
    inline float calcTime() // << this should be dynamically settable somehow
    {
        return pixelsToTime( getMessage("/x").getFloat() );
    }
    
    inline bool hitTestTime( float t )
    {
        float start = getTime();
        return t >= start && t <= ( start + getDuration() );
    }
    
    inline static t_sym_type symTypeFromString( string s ) {
        if ( s == "path" ) return PATH;
        else if ( s == "circle" ) return CIRCLE;
        else if ( s == "triangle" ) return TRIANGLE;
        else if ( s == "rectangle" ) return RECTANGLE;
        else if ( s == "staff" ) return STAFF;
        else if ( s == "group" ) return GROUP;
        else return UNKNOWN;
    }
    
    inline static string stringFromSymType( t_sym_type st ) {
        if ( st == PATH ) return "path";
        else if ( st == CIRCLE ) return "circle";
        else if ( st == TRIANGLE  ) return "triangle" ;
        else if ( st == RECTANGLE  ) return "rectangle";
        else if ( st == STAFF ) return "staff" ;
        else if ( st == GROUP ) return "group";
        else return "<none>";
    }
        
private:
    
    float       m_pixels_to_time = 0.01f;
    float       m_time_to_pixels = 100.0f;

};

