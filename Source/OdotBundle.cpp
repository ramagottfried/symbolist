#include "OdotBundle.hpp"
#include "osc_bundle_iterator_u.h"
#include "osc_strfmt.h"
#include "osc_match.h"

OdotBundle::OdotBundle()
{
    ptr = odot::newOdotBundlePtr();
//    D_(std::cout << "new bundle " << &ptr << " " << ptr.get() << std::endl;)
}

OdotBundle::OdotBundle( const OdotBundle& src )
{
//    D_(cout << __func__ << "copy \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, src.ptr.get() );
    ptr = odot::newOdotBundlePtr( b );
}

OdotBundle::OdotBundle( const t_osc_bndl_u * src )
{
//    D_(cout << __func__  << "copy from odot pointer \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, (t_osc_bndl_u *)src );
    ptr = odot::newOdotBundlePtr( b );
}

OdotBundle::OdotBundle( const OdotMessage& msg )
{
    ptr = odot::newOdotBundlePtr();
    addMessage( msg );
}

OdotBundle& OdotBundle::operator=( const OdotBundle& src )
{
//    D_(cout << __func__  << "copy= \n";)
    
    if( this != &src )
    {
        t_osc_bndl_u *b = osc_bundle_u_alloc();
        osc_bundle_u_copy( &b, (t_osc_bndl_u *)src.ptr.get() );
        ptr = odot::newOdotBundlePtr( b );
    }
    
    return *this;
}

OdotBundle::OdotBundle( vector<OdotMessage> msg_vec )
{
    OdotBundle();
    addMessage( msg_vec );
}

void OdotBundle::addMessage( vector<OdotMessage> msg_vec )
{
    for( int i = 0; i < msg_vec.size(); i++ )
        addMessage( msg_vec[i] );
}

void OdotBundle::addMessage( const OdotMessage& msg )
{
    // NOTE: releasing ownership of the memory here into the bundle
    // the bundle is wrapped in another unique_ptr, so we don't have to worry about it
    // but the sending OdotMessage will be empty...
    // ... make copy instead? ok yes, doing that...
    OdotMessage msg_cpy( msg );
    osc_bundle_u_addMsgWithoutDups( ptr.get(), msg_cpy.release() );
}

void OdotBundle::clear()
{
    osc_bundle_u_clear( ptr.get() );
}

void OdotBundle::print( int level ) const
{
    string indent = "";
    for( int i = 0; i < level; i++ )
        indent += ".\t";
    
    cout << indent << "==== ODOT BUNDLE ====" << endl;
    cout << indent << "   ( " << ptr.get() << " )" << endl;

    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        cout << indent << osc_message_u_getAddress(msg);

        char buf[256];
        char *buf_ptr = buf;
        int argcount = osc_message_u_getArgCount(msg);
        for( int i = 0; i < argcount; i++ )
        {
            t_osc_atom_u *a = osc_message_u_getArg(msg, i);
            if( osc_atom_u_getTypetag(a) == OSC_BUNDLE_TYPETAG )
            {
                cout << indent << "\t{ \n";
                OdotBundle( osc_atom_u_getBndl(a) ).print( level+1 );
                cout << indent << " } ";
            }
            else
            {
                osc_atom_u_getString( a, 256, &buf_ptr );
                cout << "\t" << buf_ptr;
            }
        }
        cout << endl;
    }
    osc_bndl_it_u_destroy(it);
    
    cout << indent << "====-===-======-====" << endl;

}

bool OdotBundle::addressExists( const string& address )
{
    int res;
    osc_bundle_u_addressExists( ptr.get(), (char *)address.c_str(), 1, &res );
    return (res == 1);
}

/* get first OSC Messages matching this address (full match) */
OdotMessage OdotBundle::getMessage( const char * address )
{

    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *current_message = osc_bndl_it_u_next(it);
        int po, ao;
        int r = osc_match( address, osc_message_u_getAddress( current_message ), &po, &ao );
        
        if(r == (OSC_MATCH_ADDRESS_COMPLETE | OSC_MATCH_PATTERN_COMPLETE))
        {
            osc_bndl_it_u_destroy(it);
            return OdotMessage( current_message );
        }
    }
    osc_bndl_it_u_destroy(it);
    return OdotMessage();

}

vector<OdotMessage> OdotBundle::matchAddress( const char * address, int fullmatch )
{
    vector<OdotMessage> ar;
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *current_message = osc_bndl_it_u_next(it);
        int po, ao;
        int r = osc_match( address, osc_message_u_getAddress( current_message ), &po, &ao );
        if( fullmatch )
        {
            if(r != (OSC_MATCH_ADDRESS_COMPLETE | OSC_MATCH_PATTERN_COMPLETE))
            {
                continue;
            }
        }
        else
        {
            if(r == 0 || (((r & OSC_MATCH_PATTERN_COMPLETE) == 0) && address[po] != '/'))
            {
                continue;
            }
        }
        ar.emplace_back( OdotMessage( current_message ) );
    }
    osc_bndl_it_u_destroy(it);
    return ar;
    
}






