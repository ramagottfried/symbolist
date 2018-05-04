#include "OdotBundle_s.hpp"
#include "OdotBundle.hpp"
#include "osc_bundle_iterator_s.h"
#include "osc_mem.h"

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

OdotBundle OdotBundle_s::deserialize() const
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

void OdotBundle_s::applyExpr( const OdotExpr& expr )
{
    t_osc_bndl_s * bundle = get_o_ptr();
    char *copy = NULL;
    long copylen = osc_bundle_s_getLen( bundle );
    copy = (char *)osc_mem_alloc( copylen );
    if(copy)
    {
        memcpy(copy, osc_bundle_s_getPtr( bundle ), copylen);
        
        int error = 0;
        t_osc_expr *f = expr.get_o_ptr();
        while(f){
            t_osc_atom_ar_u *av = NULL;
            error = osc_expr_eval( f, &copylen, &copy, &av, this );
            if(av){
                osc_atom_array_u_free(av);
            }
            if(error)
            {
                break;
            }
            f = osc_expr_next(f);
        }
        
        if( !error )
        {
            ptr = odot::newOdotBundlePtr_s( osc_bundle_s_alloc(copylen, copy) );
        }
        
        //osc_mem_free(copy);
    }
}



