

#include "ext.h"
#include "ext_obex.h"
#include "symbolist.hpp"

typedef struct _symbolist
{
    t_object    ob;
    
    void*       symbolist_window;
    void*       m_qelem_open;
    
} t_symbolist;

t_class	*symbolist_class = NULL;


BEGIN_USING_C_LINKAGE
void		*symbolist_new(t_symbol *s, long argc, t_atom *argv);
void		symbolist_free(t_symbolist *x);
END_USING_C_LINKAGE



void symbolist_qelem_open_window( t_symbolist *x )
{
    object_post((t_object *)x, "opening window from %s thread", (isr() == 0 ? "main" : "timer") );
    x->symbolist_window = symbolistNewWindow();
    
}

void symbolist_open_window( t_symbolist *x )
{
    qelem_set(x->m_qelem_open);
}

BEGIN_USING_C_LINKAGE
void symbolist_free(t_symbolist *x)
{
    if( x->symbolist_window )
    {
        symbolistCloseWindow( x->symbolist_window );
    }
    
    qelem_free(x->m_qelem_open);
}

void *symbolist_new(t_symbol *s, long argc, t_atom *argv)
{
    t_symbolist *x;

    x = (t_symbolist *)object_alloc( symbolist_class );
    if( x )
    {
        x->symbolist_window = NULL;
        x->m_qelem_open = qelem_new((t_object *)x, (method)symbolist_qelem_open_window);
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
    
    
    class_addmethod(c, (method)symbolist_open_window, "open", 0);

    
    class_register(CLASS_BOX, c);
    symbolist_class = c;
    
    return;
}
END_USING_C_LINKAGE