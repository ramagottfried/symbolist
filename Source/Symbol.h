#pragma once

#include "JuceHeader.h"
#include "types.h"
#include "OdotBundle.hpp"
#include "symbolist-utils.hpp"

using namespace std;

/**
 * Describes the structure of all symbolist's symbols.
 * This class inherits from the OdotBundle class, meaning that all symbols
 * are represented by an underlying OSC (Open Sound Control) bundle (Odot being an implementation of
 * the OSC format, more information about the OSC at http://opensoundcontrol.org/introduction-osc ).
 * Even if each symbol of symbolist is an OSC bundle and therefore can contain any type
 * of OSC messages, there are some messages which are recurrent in the symbol structure, like
 * "/id", "/name" or "/type". Thus, the Symbol class proposes a set of getters ans setters methods
 * to handle these messages.
 */
class Symbol : public OdotBundle
{
public:
    
    using OdotBundle::OdotBundle;
	
	 OdotBundle_s exportToOSC();
     void         importFromOSC( OdotBundle_s& s_bundle );
	
	/************************************
	 *         GETTERS & SETTERS        *
	 ************************************/
	float getTime() const;
    float getDuration() const;
    float getEndTime() const;
    string getName() const;
    string getID();
    string getStaff();
    string getType();
	
    void setTypeXYWH(const string & type, float x, float y, float w, float h);
	void setPosition( const Point<float> pos );

    void setTimeAndDurationFromRelPix( const float start_x, const float dur_x );
    void setTimeAndDuration( const float start_t, const float dur_t );

	/**************************************
	 *           TIME CONVERSION          *
	 **************************************/
	
    static inline float pixelsToTime( const float f )
    {
        return f * Symbol::m_pixels_to_time;
    }
    
    static inline float timeToPixels( const float t )
    {
        return t * Symbol::m_time_to_pixels;
    }
    
    inline bool hitTestTime( float t )
    {
        float start = getTime();
        return t >= start && t <= ( start + getDuration() );
    }
	
	/************************************
	 *           ID GENERATION          *
	 ************************************/
	
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
    
    static float m_pixels_to_time;
    static float m_time_to_pixels;

};

