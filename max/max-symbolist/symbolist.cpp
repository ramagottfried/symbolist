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
    char *ptr = NULL;
    ptr = (char *)atom_getlong(argv + 1);
    
    if( len && ptr )
    {
        t_osc_bndl_s *s_bndl = osc_bundle_s_alloc(len, ptr);
        if( s_bndl )
        {
            symbolistSetSymbols( x->symbolist_handler, s_bndl );
            osc_bundle_s_free(s_bndl);
        }
    }
    
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
