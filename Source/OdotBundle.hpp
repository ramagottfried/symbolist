#pragma once

#include <stdio.h>
#include <iostream>
#include <string>

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
    
    OdotBundle( const string& str );
    
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
    
    /* ======= Setters ======= */
    void clear();
    
    /**
     * Set an OdotBundle from the text passed in parameter
     * which represents an OSC bundle formatted in JSON.
     *
     * @param textToParse the string representing the OSC bundle
     *                      formatted in JSON.
     *
     * @throws invalid_argument If the string in parameter is not a
     *                            well-formed OSC bundle. Meaning it is
     *                            not formatted as intended in the o.compose
     *                          from the o. library.
     */
    void setFromString( const string& textToParse );
    
    /**
     * Creates an OdotBundle from the text contained in a file.
     *
     * @param oscFilePath the path to the file containing an OSC
     *                    bundle as text data.
     *
     * @throws invalid_argument If the string in parameter is not a
     *                            well-formed OSC bundle. Meaning it is
     *                            not formatted as intended in the o.compose
     *                          from the o. library.
     *                            If the file path in parameter is an invalid
     *                            one.
     *
     */
    void setFromFile(const string& oscFilePath);
    
    /**
     * Union values with another bundle
     *
     * @param other     bundle to union with
     *
     * @param passive   if set to true, union will give precedence to the other bundle
     */
    void unionWith( const OdotBundle& other, bool passive = false );
    
    
    
    /* ======= Add Messages ======= */

    void addMessage( const OdotMessage& msg );
    void addMessage( t_osc_msg_u * msg );

    template <typename... Ts>
    inline void addMessage (const char * address, Ts&&... args)
    {
        addMessage( OdotMessage( address, args... ).release() );
    }

    template <typename... Ts>
    inline void addMessage (const string& address, Ts&&... args)
    {
        addMessage( address.c_str(), args... );
    }
    void addMessage( vector<OdotMessage > msg_vec );
    
    /* ======= Expressions ======= */
    
    void applyExpr( const OdotExpr& expr );
    inline void applyExpr( const string& expr ) { applyExpr( OdotExpr(expr) ); }
    inline void applyExpr( const char * expr ) { applyExpr( OdotExpr(expr) ); }
    
    /* ======= Get Messages ======= */

    OdotMessage getMessage( const char * address ) const;
    OdotMessage getMessage( const string& address ) const { return getMessage( address.c_str() ); }
    
    /**
     *  Recursively search all subbundles for address and return containing subbundle
     *
     *  @param address      address to find
     *
     *  @returns            Bundle containing first matching message
     */
    OdotBundle getBundleContainingMessage( const char * address ) const;
    
    /**
     *  Recursively search all subbundles for address/value pair and return containing subbundle
     *
     *  @param msg      OdotMessage to find
     *
     *  @returns        Bundle containing first matching message
     */
    OdotBundle getBundleContainingMessage( OdotMessage& msg ) const;
    //OdotBundle getBundleContainingMessageValue( const char * address, OdotAtom& val ) const;

    vector<OdotMessage> getMessageArray() const;

    vector<OdotMessage> matchAddress( const char * address, int fullmatch = 1) const;
    inline vector<OdotMessage> matchAddress( const string& address, int fullmatch = 1) const { return matchAddress(address.c_str(), fullmatch); }
    
    /* ======= Info ======= */

    bool operator==( const OdotBundle& src );
    
    int size() const { return osc_bundle_u_getMsgCount( ptr.get() ); }
    
    bool unbound(){ return get_o_ptr() == nullptr; }
    bool addressExists( const char * address ) const;
    bool addressExists( const string& address ) const;
    
    /* ======= Debug ======= */

    void print( int level = 0 ) const;
    void print_imp( t_osc_bndl_u * bndl, int level = 0 ) const;
    
    void getPrintString(string &str, int level = 0 );
    void getPrintStringArray(vector<string >& str, int level = 0 );

    /* ======= Export ======= */
    
    inline OdotBundle_s serialize(){ return OdotBundle_s( osc_bundle_u_serialize( ptr.get() ) ); }
    
    // n.b. caller must free this pointer!
    inline t_osc_bndl_s * get_t_osc_bndl_s(){ return osc_bundle_u_serialize( ptr.get() ); }
    
    string getJSON();
    
    /* ======= pointers ======= */
    
    inline const t_osc_bndl_u * get_o_ptr() const { return ptr.get(); }
    inline t_osc_bndl_u * release(){ return ptr.release(); }
    
private:
    
    odot::OdotBundlePtr ptr;
    
    
    OdotBundle getBundleContainingMessage_imp( t_osc_bndl_u * bndl, const char * address ) const;
    OdotBundle getBundleContainingMessage_imp( t_osc_bndl_u * bndl, OdotMessage& msg ) const;

};

/*
 t_osc_msg_u * lookup_osc_msg_u( const char * address );
 void deserializeMerge( const t_osc_bundle_s *src );
 */

