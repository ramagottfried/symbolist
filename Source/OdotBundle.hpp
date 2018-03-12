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
    void addMessage( const OdotMessage& msg );
    
    OdotMessage getMessage( const char * address ) const;
    OdotMessage getMessage( const string& address ) const { return getMessage( address.c_str() ); }
    vector<OdotMessage> matchAddress( const char * address, int fullmatch = 1);
    
    void clear();
    void print( int level = 0 ) const;
    
    bool addressExists( const char * address );
    bool addressExists( const string& address );
    
    inline const t_osc_bndl_u * get_o_ptr() { return ptr.get(); }
    inline t_osc_bndl_u * release(){ return ptr.release(); }
    
    inline OdotBundle_s serialize(){ return OdotBundle_s( osc_bundle_u_serialize( ptr.get() ) ); }

private:
    
    odot::OdotBundlePtr ptr;
    
};


