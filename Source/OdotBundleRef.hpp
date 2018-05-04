#pragma once
#include <iostream>
#include <stdexcept>

#include "osc_bundle_u.h"
#include "osc_message_u.h"

#include "OdotPointers.h"
#include "OdotMessage.hpp"
#include "OdotAtom.hpp"
#include "OdotBundle_s.hpp"
#include "OdotExpr.hpp"

using namespace std;

class OdotBundleRef
{
public:
    OdotBundleRef(){}
    OdotBundleRef( t_osc_bndl_u *src );
    OdotBundleRef( const OdotBundleRef& src );
    OdotBundleRef( const OdotBundleRef* src );
    
    OdotBundleRef( const OdotBundle& src );
    OdotBundleRef( const OdotBundle* src );
    
    OdotBundleRef( const OdotBundle_s& src );
    OdotBundleRef( const t_osc_bndl_s *src );
    
    OdotBundleRef( const OdotMessage& msg );
    OdotBundleRef( vector<OdotMessage> msg_vec );
    
    OdotBundleRef& operator= ( const OdotBundleRef& src );
    OdotBundleRef& operator= ( OdotBundleRef& src );
    OdotBundleRef& operator= ( const OdotBundle& src );
    
    OdotBundleRef( OdotBundleRef&& src ) = default;
    OdotBundleRef& operator=( OdotBundleRef&& src ) = default;
    
    OdotBundleRef( const string& str );
    
    ~OdotBundleRef(){}
    
    void setFromString( const string& str );
    
    void addMessage( const OdotMessage& msg );
    void addMessage( t_osc_msg_u * msg );

    template <typename... Ts>
    inline void addMessage (const char * address, Ts&&... args)
    {
        addMessage( OdotMessage( address, args... ).release() );
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
    
    int size() const { return osc_bundle_u_getMsgCount( get_o_ptr() ); }
    
    void clear();
    void print( int level = 0 ) const;
    void getPrintString(string &str, int level = 0 );
    void getPrintStringArray(vector<string>& str, int level = 0 );
    
    bool addressExists( const char * address ) const;
    bool addressExists( const string& address ) const;
    
    
    inline OdotBundle_s serialize(){ return OdotBundle_s( osc_bundle_u_serialize( get_o_ptr() ) ); }
    
    // n.b. caller must free this pointer!
    inline t_osc_bndl_s * get_t_osc_bndl_s(){ return osc_bundle_u_serialize( get_o_ptr() ); }
    
    /**
     * Union values with another bundle
     *
     * @param passive   if set to true, union will give precedence to the other bundle
     */
    void unionWith( const OdotBundleRef& other, bool passive = false );
    
    void applyExpr( const OdotExpr& expr );
    inline void applyExpr( const string& expr ) { applyExpr( OdotExpr(expr) ); }
    inline void applyExpr( const char * expr ) { applyExpr( OdotExpr(expr) ); }

    string getJSON();
    
    virtual inline t_osc_bndl_u * get_o_ptr() const { return m_bndl; }
    virtual inline void set_o_ptr( t_osc_bndl_u * src ) { m_bndl = src; }
    
protected:
    
    t_osc_bndl_u *  m_bndl = nullptr;

};
