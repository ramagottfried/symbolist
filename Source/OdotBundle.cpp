#include "OdotBundle.hpp"

OdotBundle::OdotBundle()
{
    ptr = odot::newOdotBundlePtr();
    m_bndl = ptr.get();
//    D_(std::cout << this << " new bundle " << &ptr << " " << ptr.get() << std::endl;)
}

OdotBundle::OdotBundle( const OdotBundle& src )
{
//    D_(cout << __func__ << "copy \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, src.ptr.get() );
    ptr = odot::newOdotBundlePtr( b );
    m_bndl = ptr.get();
}

OdotBundle::OdotBundle( const OdotBundle* src )
{
    //    D_(cout << __func__ << "copy \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, src->ptr.get() );
    ptr = odot::newOdotBundlePtr( b );
    m_bndl = ptr.get();
}

OdotBundle::OdotBundle( const t_osc_bndl_u * src )
{
//    D_(cout << __func__  << "copy from odot pointer \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, (t_osc_bndl_u *)src );
    ptr = odot::newOdotBundlePtr( b );
    m_bndl = ptr.get();
}

OdotBundle::OdotBundle( const OdotBundleRef& src )
{
    OdotBundle b( src.get_o_ptr() );
    ptr = odot::newOdotBundlePtr( b.release() );
    m_bndl = ptr.get();
}

OdotBundle::OdotBundle( const OdotBundle_s& src )
{
    OdotBundle b( src.get_o_ptr() );
    ptr = odot::newOdotBundlePtr( b.release() );
    m_bndl = ptr.get();
}

OdotBundle::OdotBundle( const t_osc_bndl_s * src )
{
    ptr = odot::newOdotBundlePtr(osc_bundle_s_deserialize(osc_bundle_s_getLen((t_osc_bndl_s *)src),
                                                           osc_bundle_s_getPtr((t_osc_bndl_s *)src)));
    m_bndl = ptr.get();
}

OdotBundle::OdotBundle( const OdotMessage& msg )
{
    ptr = odot::newOdotBundlePtr();
    m_bndl = ptr.get();
    
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
        m_bndl = ptr.get();
    }
    return *this;
}

OdotBundle::OdotBundle( const string& str ) : OdotBundle()
{
    setFromString( str );
    m_bndl = ptr.get();
}

OdotBundle::OdotBundle( vector<OdotMessage> msg_vec ) : OdotBundle()
{
    addMessage( msg_vec );
    m_bndl = ptr.get();
}

t_osc_bndl_u * OdotBundle::get_o_ptr() const
{
    if( ptr.get() )
        return ptr.get();
    else
        return OdotBundleRef::get_o_ptr();
}

void OdotBundle::set_o_ptr( t_osc_bndl_u * src )
{
    if( ptr.get() )
        ptr = odot::newOdotBundlePtr( src );
    else
        OdotBundleRef::set_o_ptr( src );
}

bool OdotBundle::ownsBundle() const
{
    return (ptr.get() != nullptr );
    
}

t_osc_bndl_u * OdotBundle::release()
{
    if( !ptr.get() )
        throw std::runtime_error("error: cannot release null bundle");
    
    return ptr.release();
    
}


OdotBundleRef OdotBundle::getRef()
{
    return OdotBundleRef( get_o_ptr() );
}
