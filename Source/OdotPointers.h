
#pragma once

#include <memory>
#include "osc_message_u.h"
#include "osc_atom_u.h"
#include "osc_bundle_u.h"
#include "osc_bundle_s.h"
#include "osc_expr.h"

#ifdef DEBUG
#define D_(x) x
#else
#define D_(x)
#endif

using namespace std;

namespace odot
{
    struct OdotPtrDeleter
    {
        void operator()(t_osc_msg_u*  ptr)
        {
            if (ptr)
                osc_message_u_free(ptr);
            
        }
        
        void operator()(t_osc_atom_u*  ptr)
        {
            if (ptr)
                osc_atom_u_free(ptr);
        
        }
        void operator()(t_osc_bndl_u* ptr)
        {
            cout << "freeing " << ptr << endl;
            if (ptr)
                osc_bundle_u_free(ptr);
        }
        
        void operator()(t_osc_bndl_s* ptr)
        {
            if (ptr)
                osc_bundle_s_deepFree(ptr);
        }
        
        void operator()(t_osc_expr* ptr)
        {
            if (ptr)
                osc_expr_free(ptr);
        }
    };

    using OdotMessagePtr = std::unique_ptr<t_osc_msg_u,  OdotPtrDeleter>;
    using OdotAtomPtr    = std::unique_ptr<t_osc_atom_u, OdotPtrDeleter>;
    using OdotBundlePtr  = std::unique_ptr<t_osc_bndl_u, OdotPtrDeleter>;
    using OdotBundlePtr_s  = std::unique_ptr<t_osc_bndl_s, OdotPtrDeleter>;
    using OdotExprPtr      = std::unique_ptr<t_osc_expr, OdotPtrDeleter>;
    
    static inline OdotMessagePtr newOdotMessagePtr() {
        return OdotMessagePtr( osc_message_u_alloc(), OdotPtrDeleter() );
    }

    static inline OdotMessagePtr newOdotMessagePtr( t_osc_msg_u * src ) {
        return OdotMessagePtr( src, OdotPtrDeleter() );
    }

    static inline OdotAtomPtr newOdotAtomPtr() {
        return OdotAtomPtr( osc_atom_u_alloc(), OdotPtrDeleter() );
    }
    
    static inline OdotAtomPtr newOdotAtomPtr( t_osc_atom_u * src ) {
        return OdotAtomPtr( src, OdotPtrDeleter() );
    }
    
    static inline OdotBundlePtr newOdotBundlePtr() {
        return OdotBundlePtr( osc_bundle_u_alloc(), OdotPtrDeleter() );
    }
    
    static inline OdotBundlePtr newOdotBundlePtr( t_osc_bndl_u * src ) {
        return OdotBundlePtr( src, OdotPtrDeleter() );
    }
    
    static inline OdotBundlePtr_s newOdotBundlePtr_s() {
        return OdotBundlePtr_s( osc_bundle_s_allocEmptyBundle(), OdotPtrDeleter() );
    }
    
    static inline OdotBundlePtr_s newOdotBundlePtr_s( t_osc_bndl_s * src ) {
        return OdotBundlePtr_s( src, OdotPtrDeleter() );
    }

    static inline OdotExprPtr newOdotExprPtr() {
        return OdotExprPtr( osc_expr_alloc(), OdotPtrDeleter() );
    }
    
    static inline OdotExprPtr newOdotExprPtr( t_osc_expr * src ) {
        return OdotExprPtr( src, OdotPtrDeleter() );
    }

    
}
