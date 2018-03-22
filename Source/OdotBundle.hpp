#pragma once
#include <iostream>
#include "osc_bundle_u.h"
#include "OdotPointers.h"
#include "OdotMessage.hpp"
#include "OdotAtom.hpp"
#include "OdotBundle_s.hpp"

using namespace std;

class OdotBundle
{
public:
    OdotBundle();
    OdotBundle( const OdotBundle& src );
    OdotBundle( const t_osc_bndl_u *src );
    OdotBundle( const OdotBundle_s& src );
    OdotBundle( const t_osc_bndl_s *src );
    OdotBundle( const OdotMessage& msg );
    OdotBundle( vector<OdotMessage> msg_vec );
    
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
    void addMessage( vector<OdotMessage> msg_vec );

    OdotMessage getMessage( const char * address ) const;
    OdotMessage getMessage( const string& address ) const { return getMessage( address.c_str() ); }
    vector<OdotMessage> getMessageArray() const;

    vector<OdotMessage> matchAddress( const char * address, int fullmatch = 1) const;
    inline vector<OdotMessage> matchAddress( const string& address, int fullmatch = 1) const { return matchAddress(address.c_str(), fullmatch); }
    
    int size() const { return osc_bundle_u_getMsgCount( ptr.get() ); }
    
    void clear();
    void print( int level = 0 ) const;
    void getPrintString(string &str, int level = 0 );
    void getPrintStringArray(vector<string>& str, int level = 0 );

    bool addressExists( const char * address );
    bool addressExists( const string& address );
    
    inline const t_osc_bndl_u * get_o_ptr() const { return ptr.get(); }
    inline t_osc_bndl_u * release(){ return ptr.release(); }
    
    inline OdotBundle_s serialize(){ return OdotBundle_s( osc_bundle_u_serialize( ptr.get() ) ); }
    
    // n.b. caller must free this pointer!
    inline t_osc_bndl_s * get_t_osc_bndl_s(){ return osc_bundle_u_serialize( ptr.get() ); }

    void unionWith( const OdotBundle& other );
    
private:
    
    odot::OdotBundlePtr ptr;
    
};

/*
 t_osc_msg_u * lookup_osc_msg_u( const char * address );
 void deserializeMerge( const t_osc_bundle_s *src );
 */

