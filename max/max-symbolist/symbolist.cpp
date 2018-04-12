#include "ext.h"
#include "ext_critical.h"

#include "osc.h"
#include "osc_bundle_u.h"
#include "osc_atom_u.h"
#include "osc_bundle_iterator_u.h"
#include "osc_message_iterator_u.h"
#include "osc_bundle_s.h"
#include "osc_message_s.h"
#include "osc_bundle_iterator_s.h"
#include "osc_mem.h"

#include "omax_util.h"

#include "symbolist.hpp"

typedef struct _symbolist
{
    t_object    ob;

    double      current_time;
    
    void*       symbolist_handler;
    
    void*       m_qelem_open;
    void*       m_qelem_setTime;

    void*       player_outlet;
    void*       dump_outlet;
    
    bool        window_is_open;
    
    t_critical  lock;
    
} t_symbolist;

t_class	*symbolist_class = NULL;
std::vector<t_symbolist*> symbolist_objects; // global array of object instances for callback reference

t_symbol *symbolist_ps_FullPacket;

BEGIN_USING_C_LINKAGE
void		*symbolist_new(t_symbol *s, long argc, t_atom *argv);
void		symbolist_free(t_symbolist *x);
END_USING_C_LINKAGE

void symbolist_outletOSC(void *outlet, long len, char *ptr)
{
    t_atom out[2];
    atom_setlong(out, len);
    atom_setlong(out + 1, (long)ptr);
    outlet_anything(outlet, symbolist_ps_FullPacket, 2, out);
}

void symbolist_closecallback ( void * sc )
{
    for( auto x : symbolist_objects )
    {
        if( x->symbolist_handler == sc )
        {
            x->window_is_open = false;
        }
    }
}

void symbolist_updatecallback( void * sc, int n )
{
    for( auto x : symbolist_objects )
    {
        if( x->symbolist_handler == sc )
        {
            t_object *jp;
            t_max_err err = object_obex_lookup(x, gensym("#P"), (t_object **)&jp);
            if( !err )
                jpatcher_set_dirty(jp, true);
            
        }
    }
    
}

void symbolist_qset_time( t_symbolist *x )
{
    critical_enter(x->lock);
    double time = x->current_time;
    critical_exit(x->lock);
    
    symbolistSetTime( x->symbolist_handler, time );
}

void symbolist_getSymbols_at_time( t_symbolist *x, double time )
{
    critical_enter(x->lock);
    x->current_time = time;
    critical_exit(x->lock);
    
    qelem_set( x->m_qelem_setTime );

    t_osc_bndl_s* bndl = symbolistGetSymbolsAtTime( x->symbolist_handler, time);
    if( bndl )
        symbolist_outletOSC( x->player_outlet, osc_bundle_s_getLen(bndl), osc_bundle_s_getPtr(bndl) );

}

void symbolist_getDuration( t_symbolist *x )
{
    t_osc_bndl_s *bndl = symbolistGetDurationBundle( x->symbolist_handler);
    if( bndl )
        symbolist_outletOSC( x->player_outlet, osc_bundle_s_getLen(bndl), osc_bundle_s_getPtr(bndl)  );
    
}

t_osc_bundle_u *odowncast_iterBundle(t_symbolist *x, t_osc_bundle_u *b, t_osc_timetag *timetag)
{
    t_osc_bndl_it_u *bit = osc_bndl_it_u_get(b);
    while(osc_bndl_it_u_hasNext(bit))
    {
        t_osc_msg_u *m = osc_bndl_it_u_next(bit);
        t_osc_msg_it_u *mit = osc_msg_it_u_get(m);
        while(osc_msg_it_u_hasNext(mit)){
            t_osc_atom_u *a = osc_msg_it_u_next(mit);
            int i = 0;
            switch(osc_atom_u_getTypetag(a)){
                case 'c':
                case 'C':
                case 'I':
                case 'h':
                case 'H':
                case 'u':
                case 'U':
                case 'N':
                case 'T':
                case 'F':
                    osc_atom_u_setInt32(a, osc_atom_u_getInt32(a));
                    break;
                case 'd':
                    osc_atom_u_setFloat(a, osc_atom_u_getFloat(a));
                    break;
                case OSC_BUNDLE_TYPETAG:
                {
                    // get pointer to bundle (not a copy)
                    t_osc_bundle_u *sub_b_u = osc_atom_u_getBndl(a);
                    
                    sub_b_u = odowncast_iterBundle(x, sub_b_u, timetag);
                    
                    // after downcasting, and blobbing subbundles, serialize and convert to blob
                    t_osc_bundle_s *sub_b_s = osc_bundle_u_serialize(sub_b_u);
                    if( sub_b_s )
                    {
                        // temp atom for conversion
                        t_osc_atom_u *tmp_atom_s_bnd = osc_atom_u_alloc();
                        osc_atom_u_setBndl_s(tmp_atom_s_bnd, osc_bundle_s_getLen(sub_b_s), osc_bundle_s_getPtr(sub_b_s));
                        
                        // make the blob
                        char *blob = NULL;
                        int32_t blob_l;
                        osc_atom_u_getBlobCopy(tmp_atom_s_bnd, &blob_l, &blob); //<< internally checks for and copies bundle into blob format
                        
                        if( blob )
                        {
                            // transfer to the current atom (a)
                            // internally calls atom_u_clear/free which frees our sub_b_u pointer, so we don't have to do that
                            osc_atom_u_setBlob(a, blob);
                            
                            // release the blob
                            osc_mem_free(blob);
                            blob = NULL;
                        }
                        
                        // release temp bundle atom
                        osc_atom_u_free(tmp_atom_s_bnd);
                        osc_bundle_s_deepFree(sub_b_s);
                    }
                    
                    
                }
                break;
            }
            i++;
        }
        osc_msg_it_u_destroy(mit);
    }
    osc_bndl_it_u_destroy(bit);
    return b;
}

void symbolist_setSymbol( t_symbolist *x, t_symbol *msg, int argc, t_atom *argv )
{
    if(argc != 2){
        object_error((t_object *)x, "expected 2 arguments but got %d", argc);
        return;
    }
    if(atom_gettype(argv) != A_LONG){
        object_error((t_object *)x, "argument 1 should be an int");
        return;
    }
    if(atom_gettype(argv + 1) != A_LONG){
        object_error((t_object *)x, "argument 2 should be an int");
        return;
    }
    long len = atom_getlong(argv);
    char *ptr = (char *)atom_getlong(argv + 1);
    
    t_osc_bndl_u *b = osc_bundle_s_deserialize(len, ptr);
    if(!b){
        object_error((t_object *)x, "invalid OSC packet");
        return;
    }
    
    
 //   t_osc_msg_ar_u *symbols = osc_bundle_u_lookupAddress( b , "/symbol", 0);
    
    
    
    // remove downcast once symbolist odot is working
    t_osc_timetag timetag = OSC_TIMETAG_NULL;
    t_osc_bundle_u *blobbed_b = odowncast_iterBundle(x, b, &timetag);
    if( !blobbed_b )
    {
        object_error((t_object *)x, "invalid OSC packet");
        osc_bundle_u_free(b);
        return;
    }

    t_osc_bndl_s *s_bndl = osc_bundle_u_serialize( blobbed_b );

    if( s_bndl )
    {
        symbolistSetOneSymbol( x->symbolist_handler, s_bndl );
        osc_bundle_s_deepFree(s_bndl);
    }
    
    osc_bundle_u_free( blobbed_b );
}

void symbolist_clearScore( t_symbolist *x )
{
    symbolistClearScore( x->symbolist_handler );
}

void symbolist_getScoreBundle( t_symbolist *x )
{
    
    t_osc_bndl_s* bndl = symbolistGetScoreBundle( x->symbolist_handler );
    if( bndl )
        symbolist_outletOSC( x->dump_outlet, osc_bundle_s_getLen(bndl), osc_bundle_s_getPtr(bndl) );
    
}

void symbolist_get_symbol( t_symbolist *x, int num)
{
    if( num >= 0 && num < symbolistGetNumSymbols( x->symbolist_handler ) )
    {
        t_osc_bndl_s* bndl = symbolistGetSymbol( x->symbolist_handler, num );
        symbolist_outletOSC( x->player_outlet, osc_bundle_s_getLen(bndl), osc_bundle_s_getPtr(bndl) );
    }
    else
        object_error((t_object *)x, "lookup not in range!");
    
}

void symbolist_open_window( t_symbolist *x )
{
   qelem_set(x->m_qelem_open);
}

void symbolist_qelem_open_window( t_symbolist *x )
{
    if ( !x->window_is_open )
    {
        symbolistOpenWindow( x->symbolist_handler );
         x->window_is_open = true;
    }
    else
        symbolistWindowToFront( x->symbolist_handler );

}


/*******
 *  max lifespan
 *******/

BEGIN_USING_C_LINKAGE
void symbolist_free(t_symbolist *x)
{
    if( x->symbolist_handler )
    {
        symbolistFree( x->symbolist_handler );
        x->symbolist_handler = NULL;
    }
    
    qelem_free(x->m_qelem_open);
    qelem_free(x->m_qelem_setTime);
    critical_free( x->lock );

    symbolist_objects.erase( std::remove( symbolist_objects.begin(), symbolist_objects.end(), x), symbolist_objects.end() );
    
}


void *symbolist_new(t_symbol *s, long argc, t_atom *argv)
{
    t_symbolist *x;

    x = (t_symbolist *)object_alloc( symbolist_class );
    if( x )
    {
        symbolist_objects.emplace_back( x );
        x->symbolist_handler = symbolistNew();
        
        if( !x->symbolist_handler )
        {
            object_error((t_object *)x, "could not allocate symbolist");
            return NULL;
        }
        
        x->current_time = 0;
        
        critical_new( &x->lock );
        
        x->window_is_open = 0;
        
        x->m_qelem_setTime = qelem_new((t_object *)x, (method)symbolist_qset_time);
        x->m_qelem_open = qelem_new((t_object *)x, (method)symbolist_qelem_open_window);
        x->dump_outlet = outlet_new(x, "FullPacket" );
        x->player_outlet = outlet_new(x, "FullPacket" );
        
        symbolistRegisterCloseCallback( x->symbolist_handler, &symbolist_closecallback );
        symbolistRegisterUpdateCallback( x->symbolist_handler, &symbolist_updatecallback);
        
    }
    return (x);
}

void ext_main(void* unused)
{
    t_class *c;
    
    c = class_new("symbolist",
                  (method)symbolist_new,
                  (method)symbolist_free,
                  sizeof(t_symbolist), NULL, A_GIMME, 0);
    
    class_addmethod(c, (method)symbolist_open_window,           "open",         0);
    class_addmethod(c, (method)symbolist_open_window,           "dblclick",     A_CANT, 0);
    
    class_addmethod(c, (method)symbolist_getScoreBundle,        "dump",         0);
    class_addmethod(c, (method)symbolist_setSymbol,             "FullPacket",   A_GIMME, 0);

    class_addmethod(c, (method)symbolist_getSymbols_at_time,    "time",         A_FLOAT, 0);
    class_addmethod(c, (method)symbolist_getDuration,           "getduration",  0);
    
    class_addmethod(c, (method)symbolist_get_symbol,            "getsymbol",    A_LONG, 0);
    
    class_addmethod(c, (method)symbolist_clearScore,            "clear",        0);

    
    class_register(CLASS_BOX, c);
    symbolist_class = c;
    
    
    symbolist_ps_FullPacket = gensym("FullPacket");
    return;
}
END_USING_C_LINKAGE
