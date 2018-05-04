#pragma once

#include <stdio.h>
#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include "osc_bundle_u.h"
#include "OdotPointers.h"
#include "OdotMessage.hpp"
#include "OdotAtom.hpp"
#include "OdotBundle_s.hpp"
#include "OdotExpr.hpp"

using namespace std;

class OdotBundle {

public:
    OdotBundle();
    OdotBundle( const OdotBundle& src );
    OdotBundle( const OdotBundle* src );
    OdotBundle( const t_osc_bndl_u *src );
    OdotBundle( const OdotBundle_s& src );
    OdotBundle( const t_osc_bndl_s *src );
    OdotBundle( const OdotMessage& msg );
    OdotBundle( vector<OdotMessage > msg_vec );
    
    template <typename... Ts>
    OdotBundle(const char * address, Ts&&... args)
    {
        OdotBundle();
        OdotMessage msg( address, args... );
        addMessage( msg );
    }

    OdotBundle& operator= ( const OdotBundle& src );
    
    OdotBundle( OdotBundle&& src ) = default;
    OdotBundle& operator=( OdotBundle&& src ) = default;
    
    ~OdotBundle(){}
    
    void addMessage( const OdotMessage& msg );
    
    template <typename... Ts>
    inline void addMessage (const char * address, Ts&&... args) {
        OdotMessage msg( address, args... );
        addMessage( msg );
    }

    template <typename... Ts>
    inline void addMessage (const string& address, Ts&&... args) {
        addMessage( address.c_str(), args... );
    }
    void addMessage( vector<OdotMessage > msg_vec );

    OdotMessage getMessage( const char * address ) const;
    OdotMessage getMessage( const string& address ) const { return getMessage( address.c_str() ); }
    vector<OdotMessage> getMessageArray() const;

    vector<OdotMessage> matchAddress( const char * address, int fullmatch = 1) const;
    inline vector<OdotMessage> matchAddress( const string& address, int fullmatch = 1) const { return matchAddress(address.c_str(), fullmatch); }
    
    int size() const { return osc_bundle_u_getMsgCount( ptr.get() ); }
    
    void clear();
    void print( int level = 0 ) const;
    void getPrintString(string &str, int level = 0 );
    void getPrintStringArray(vector<string >& str, int level = 0 );

    bool addressExists( const char * address ) const;
    bool addressExists( const string& address ) const;
    
    inline const t_osc_bndl_u * get_o_ptr() const { return ptr.get(); }
    inline t_osc_bndl_u * release(){ return ptr.release(); }
    
    inline OdotBundle_s serialize(){ return OdotBundle_s( osc_bundle_u_serialize( ptr.get() ) ); }
    
    // n.b. caller must free this pointer!
    inline t_osc_bndl_s * get_t_osc_bndl_s(){ return osc_bundle_u_serialize( ptr.get() ); }

    /**
     * Union values with another bundle
     *
     * @param passive   if set to true, union will give precedence to the other bundle
     */
    void unionWith( const OdotBundle& other, bool passive = false );
	
	/**
	 * Creates an OdotBundle from the text passed in parameter
	 * which represents an OSC bundle formatted in JSON.
	 * 
	 * @param textToParse the string representing the OSC bundle
	 *					  formatted in JSON.
	 *
	 * @return            a pointer to the OdotBundle created from
	 *					  the string in parameter.
	 *
	 * @throws invalid_argument If the string in parameter is not a
	 *							well-formed OSC bundle. Meaning it is
	 *							not formatted as intended in the o.compose
	 *                          from the o. library.
	 */
	static OdotBundle* createOdotBundleFromString(string textToParse);
	
	/**
	 * Creates an OdotBundle from the text contained in a file.
	 *
	 * @param oscFilePath the path to the file containing an OSC
	 *                    bundle as text data.
	 *
	 * @return            an pointer to an OdotBundle object
	 *					  created from the text contained in the file.
	 *
	 * @throws invalid_argument If the string in parameter is not a
	 *							well-formed OSC bundle. Meaning it is
	 *							not formatted as intended in the o.compose
	 *                          from the o. library.
	 *							If the file path in parameter is an invalid
	 *							one.
	 *
	 */
	static OdotBundle* createOdotBundleFromFile(string oscFilePath);
	
    void applyExpr( const OdotExpr& expr );
    inline void applyExpr( const string& expr ) { applyExpr( OdotExpr(expr) ); }
    inline void applyExpr( const char * expr ) { applyExpr( OdotExpr(expr) ); }

    string getJSON();

private:
    
    odot::OdotBundlePtr ptr;
    
};

/*
 t_osc_msg_u * lookup_osc_msg_u( const char * address );
 void deserializeMerge( const t_osc_bundle_s *src );
 */

