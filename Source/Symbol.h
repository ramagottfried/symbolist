#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "types.h"
#include "OdotBundle.hpp"
#include "symbolist-utils.hpp"

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

    OdotBundle_s exportToOSC();
    void         importFromOSC( OdotBundle_s& s_bundle );
    
    float getTime() const;
    float getDuration() const;
    float getEndTime() const;
    string getName() const;
    string getID();
    string getSaff();
    string getType();
    
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
	
	/**
	 * Says whether the id in parameter is the current symbol's id
	 * or one of its subsymbols.
	 *
	 * @param searchedId the id to compare with the current symbol's id.
	 *
	 * @return           <code>true</code>, if searchedID matches the current symbol's
	 *                   id or one of its subsymbols (if the current symbol
	 *                   is a group). <code>false</code> otherwise.
	 */
    bool idExists(string& searchedId);
	
	/**
	 * Resets all messages of address /id in the current symbol
	 * and all its nested symbols if it's a group.
	 */
	void resetAllIds();
	
private:
    
    float       m_pixels_to_time = 0.01f;
    float       m_time_to_pixels = 100.0f;

};

