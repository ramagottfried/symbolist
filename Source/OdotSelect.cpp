#include "OdotSelect.hpp"
#include "osc_bundle_iterator_u.h"

void OdotSelect::select()
{
    m_map.clear();
    recursiveSelect( m_bndl.get_o_ptr() );
}

void OdotSelect::select( const string& addr_prefix, bool fullmatch )
{
    m_map.clear();
    recursiveSelect( m_bndl.get_o_ptr(), addr_prefix );
}

void OdotSelect::select( const OdotMessage& select_msg, bool fullmatch )
{
    m_map.clear();
    recursiveSelect( m_bndl.get_o_ptr(), nullptr, select_msg );
}

void OdotSelect::recursiveSelect( const t_osc_bndl_u *bndl )
{
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( (t_osc_bndl_u *)bndl );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        const char * addr = osc_message_u_getAddress(msg);
        
        add( addr, msg );

        t_osc_atom_u *at = osc_message_u_getArg(msg, 0);
        if( osc_atom_u_getTypetag(at) == OSC_BUNDLE_TYPETAG )
        {
            t_osc_bndl_u *sub = osc_atom_u_getBndl(at);
            
            recursiveSelect( sub );
        }
    }
    osc_bndl_it_u_destroy(it);
}

void OdotSelect::recursiveSelect( const t_osc_bndl_u *bndl, const string& selector )
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
        
        add( addr, msg );

        if( osc_atom_u_getTypetag(at) == OSC_BUNDLE_TYPETAG )
        {
            t_osc_bndl_u *sub = osc_atom_u_getBndl(at);
            
            recursiveSelect( sub, selector );
        }
    }
    osc_bndl_it_u_destroy(it);
}

void OdotSelect::recursiveSelect( const t_osc_bndl_u *bndl, t_osc_msg_u * parent, const OdotMessage& select_msg )
{
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( (t_osc_bndl_u *)bndl );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        
        t_osc_atom_u *at = osc_message_u_getArg(msg, 0);
        const string addr( osc_message_u_getAddress(msg) );
        
        if( select_msg == OdotMessage(msg) && parent != nullptr )
        {
            add( osc_message_u_getAddress(parent), parent );
        }
        
        if( osc_atom_u_getTypetag(at) == OSC_BUNDLE_TYPETAG )
        {
            t_osc_bndl_u *sub = osc_atom_u_getBndl(at);
            recursiveSelect( sub, msg, select_msg );
        }
    }
    osc_bndl_it_u_destroy(it);
}


vector< OdotMessage > OdotSelect::getVector()
{
    vector< OdotMessage > vec;
    vec.reserve( m_map.size() );
    for( auto e : m_map )
    {
        vec.emplace_back( OdotMessage(e.second) );
    }
    return vec;
}

OdotBundle OdotSelect::getBundle()
{
    OdotBundle bndl;
    for( auto e : m_map )
    {
        bndl.addMessage( e.second );
    }
    return bndl;
}
