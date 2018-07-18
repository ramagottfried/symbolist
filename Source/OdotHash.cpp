
#include "OdotHash.hpp"
#include "osc_bundle_iterator_u.h"

void OdotBundleHash::rehash()
{
    //        m_map.reserve( m_bndl.size() );
    //        m_map.rehash( m_bndl.size() )
    
    // do we need to clear the map first?
    // also, do we need to specifically call reserve?
    
    m_map.clear();
    recursiveAddSubs( m_bndl.get_o_ptr() );
    
}

void OdotBundleHash::recursiveAddSubs( const t_osc_bndl_u *bndl )
{

    t_osc_bndl_it_u *it = osc_bndl_it_u_get( (t_osc_bndl_u *)bndl );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        t_osc_atom_u *at = osc_message_u_getArg(msg, 0);
        
        // could do matching here later

        if( osc_atom_u_getTypetag(at) == OSC_BUNDLE_TYPETAG )
        {
            t_osc_bndl_u *sub = osc_atom_u_getBndl(at);
            add( osc_message_u_getAddress(msg), sub );
            
            recursiveAddSubs( sub );
        }
    }
    osc_bndl_it_u_destroy(it);
}
