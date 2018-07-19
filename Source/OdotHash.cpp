
#include "OdotHash.hpp"
#include "osc_bundle_iterator_u.h"

void OdotBundleHash::select()
{
    // do we need to specifically call reserve?
    m_map.clear();
    recursiveSelect( m_bndl.get_o_ptr() );
}

void OdotBundleHash::select( const string& addr_prefix, bool fullmatch )
{
    // do we need to specifically call reserve?
    m_map.clear();
    recursiveSelect( m_bndl.get_o_ptr(), addr_prefix );
}

void OdotBundleHash::select( const OdotMessage& select_msg, bool fullmatch )
{
    // do we need to specifically call reserve?
    m_map.clear();
    recursiveSelect( m_bndl.get_o_ptr(), "", select_msg );
}

void OdotBundleHash::recursiveSelect( const t_osc_bndl_u *bndl )
{
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( (t_osc_bndl_u *)bndl );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        t_osc_atom_u *at = osc_message_u_getArg(msg, 0);
        const char * addr = osc_message_u_getAddress(msg);
        
        if( osc_atom_u_getTypetag(at) == OSC_BUNDLE_TYPETAG )
        {
            t_osc_bndl_u *sub = osc_atom_u_getBndl(at);
            add( addr, sub );
            
            recursiveSelect( sub );
        }
    }
    osc_bndl_it_u_destroy(it);
}

void OdotBundleHash::recursiveSelect( const t_osc_bndl_u *bndl, const string& selector )
{
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( (t_osc_bndl_u *)bndl );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        t_osc_atom_u *at = osc_message_u_getArg(msg, 0);
        const string addr( osc_message_u_getAddress(msg) );
        
        if( addr.find(selector) != 0 )
        {
            continue;
        }
        
        if( osc_atom_u_getTypetag(at) == OSC_BUNDLE_TYPETAG )
        {
            t_osc_bndl_u *sub = osc_atom_u_getBndl(at);
            add( addr, sub );
            
            recursiveSelect( sub, selector );
        }
    }
    osc_bndl_it_u_destroy(it);
}

void OdotBundleHash::recursiveSelect( const t_osc_bndl_u *bndl, const string& bndl_addr, const OdotMessage& select_msg )
{
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( (t_osc_bndl_u *)bndl );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        
        t_osc_atom_u *at = osc_message_u_getArg(msg, 0);
        const string addr( osc_message_u_getAddress(msg) );
        
        if( select_msg == OdotMessage(msg) && bndl != m_bndl.get_o_ptr() )
        {
            add( bndl_addr, (t_osc_bndl_u *)bndl );
        }
        
        if( osc_atom_u_getTypetag(at) == OSC_BUNDLE_TYPETAG )
        {
            t_osc_bndl_u *sub = osc_atom_u_getBndl(at);
            recursiveSelect( sub, addr, select_msg );
        }
    }
    osc_bndl_it_u_destroy(it);
}


vector< pair<string, OdotBundle> > OdotBundleHash::getVector()
{
    vector< pair<string, OdotBundle> > vec;
    vec.reserve( m_map.size() );
    for( auto e : m_map )
    {
        vec.emplace_back( e.first, OdotBundle(e.second) );
    }
    return vec;
}

OdotBundle OdotBundleHash::getBundle()
{
    OdotBundle bndl;
    for( auto e : m_map )
    {
        bndl.addMessage( e.first, e.second );
    }
    return bndl;
}
