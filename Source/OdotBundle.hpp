#pragma once

#include "OdotBundleRef.hpp"

using namespace std;

class OdotBundleRef;

class OdotBundle : public OdotBundleRef
{
public:
    OdotBundle();
    OdotBundle( const OdotBundle& src );
    OdotBundle( const OdotBundle* src );
    OdotBundle( const t_osc_bndl_u *src );
    
    OdotBundle( const OdotBundleRef& src );
    
    OdotBundle( const OdotBundle_s& src );
    OdotBundle( const t_osc_bndl_s *src );
    OdotBundle( const OdotMessage& msg );
    OdotBundle( vector<OdotMessage> msg_vec );
    
    OdotBundle( const string& str );
    
    template <typename... Ts>
    OdotBundle(const char * address, Ts&&... args) : OdotBundle()
    {
        OdotMessage msg( address, args... );
        addMessage( msg );
    }

    template <typename... Ts>
    OdotBundle(const string& address, Ts&&... args) : OdotBundle()
    {
        OdotMessage msg( address.c_str(), args... );
        addMessage( msg );
    }
    
    OdotBundle& operator= ( const OdotBundle& src );
    
    OdotBundle( OdotBundle&& src ) = default;
    OdotBundle& operator=( OdotBundle&& src ) = default;
    
    ~OdotBundle(){}
    
    virtual t_osc_bndl_u * get_o_ptr() const override;
    virtual void set_o_ptr( t_osc_bndl_u * src ) override;
    
    bool ownsBundle() const;
    
    t_osc_bndl_u * release();
    
    OdotBundleRef getRef();
    
private:
    
    odot::OdotBundlePtr ptr;
    
};

/*
 t_osc_msg_u * lookup_osc_msg_u( const char * address );
 void deserializeMerge( const t_osc_bundle_s *src );
 */

