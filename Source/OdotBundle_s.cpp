#include "OdotBundle_s.hpp"
#include "OdotBundle.hpp"

OdotBundle_s::OdotBundle_s()
{
    ptr = odot::newOdotBundlePtr_s();
    //    D_(std::cout << "new bundle " << &ptr << " " << ptr.get() << std::endl;)
}

OdotBundle_s::OdotBundle_s( const OdotBundle_s& src )
{
    //    D_(cout << __func__ << "copy \n";)
    t_osc_bndl_s *b = NULL;
    osc_bundle_s_deepCopy( &b, src.ptr.get() );
    ptr = odot::newOdotBundlePtr_s( b );
}

OdotBundle_s::OdotBundle_s( const t_osc_bndl_s * src )
{
    //    D_(cout << __func__  << "copy from odot pointer \n";)
    t_osc_bndl_s *b = NULL;
    osc_bundle_s_deepCopy( &b, (t_osc_bndl_s *)src );
    ptr = odot::newOdotBundlePtr_s( b );
}

OdotBundle_s& OdotBundle_s::operator=( const OdotBundle_s& src )
{
    if( this != &src )
    {
        t_osc_bndl_s *b = NULL;
        osc_bundle_s_deepCopy( &b, src.ptr.get() );
        ptr = odot::newOdotBundlePtr_s( b );
    }
    
    return *this;
}

OdotBundle OdotBundle_s::deserialize()
{
    return OdotBundle( osc_bundle_s_deserialize( osc_bundle_s_getLen( ptr.get() ), osc_bundle_s_getPtr( ptr.get() ) ) );
}


void OdotBundle_s::print( int level ) const
{
    string indent = "";
    for( int i = 0; i < level; i++ )
        indent += ".\t";
    
    cout << indent << "==== ODOT S BUNDLE ====" << endl;
    cout << indent << "   ( " << ptr.get() << " )" << endl;
    
    cout << osc_bundle_s_pformat( ptr.get() );
    
    cout << indent << "====-===-======-====" << endl;
    
}




