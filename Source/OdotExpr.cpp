#include "OdotExpr.hpp"
#include "osc_expr_parser.h"

OdotExpr::OdotExpr()
{
    ptr = odot::newOdotExprPtr();
    //    D_(std::cout << "new empty pointer " << &ptr << " " << ptr.get() << std::endl;)
}

OdotExpr::OdotExpr( const OdotExpr& src )
{
    t_osc_expr *dst;
    osc_expr_copy( &dst, src.ptr.get() );
    ptr = odot::newOdotExprPtr( dst );

}

OdotExpr::OdotExpr( const string& expr )
{
    t_osc_expr *e;
    t_osc_err error = osc_expr_parser_parseExpr( (char *)expr.c_str(), &e, this ); // later use the calling context for context pointer!
    if( error != OSC_ERR_NONE )
        cout << "parse error in expr " << this << endl;
    
    ptr = odot::newOdotExprPtr( e );
    
}

OdotExpr::OdotExpr( const char * expr )
{
    t_osc_expr *e;
    t_osc_err error = osc_expr_parser_parseExpr( (char *)expr, &e, this ); // later use the calling context for context pointer!
    if( error != OSC_ERR_NONE )
        cout << "parse error in expr " << this << endl;
    
    ptr = odot::newOdotExprPtr( e );
    
}

// assumes we are not responible for the pointer (to work with osc_message_u_getArg()
OdotExpr::OdotExpr( t_osc_expr * src )
{
    t_osc_expr *dst;
    osc_expr_copy(&dst, src);
    ptr = odot::newOdotExprPtr( dst );
}

OdotExpr& OdotExpr::operator=( const OdotExpr& src )
{
    if( this != &src )
    {
        t_osc_expr *dst;
        osc_expr_copy(&dst, src.ptr.get() );
        ptr = odot::newOdotExprPtr( dst );
    }
    
    return *this;
}

OdotExpr& OdotExpr::operator=( const string& expr )
{
    t_osc_expr *e;
    t_osc_err error = osc_expr_parser_parseExpr( (char *)expr.c_str(), &e, this ); // later use the calling context for context pointer!
    if( error != OSC_ERR_NONE )
        cout << "parse error in expr " << this << endl;
    
    ptr = odot::newOdotExprPtr( e );

    return *this;
}

OdotExpr& OdotExpr::operator=( const char * expr )
{
    t_osc_expr *e;
    t_osc_err error = osc_expr_parser_parseExpr( (char *)expr, &e, this ); // later use the calling context for context pointer!
    if( error != OSC_ERR_NONE )
        cout << "parse error in expr " << this << endl;
    
    ptr = odot::newOdotExprPtr( e );
    
    return *this;
}



void OdotExpr::print() const
{
    
    t_osc_expr *e = get_o_ptr();
    while ( e )
    {
        t_osc_expr_arg *arg = osc_expr_getArgs(e);
        while( arg )
        {
            switch (osc_expr_arg_getType(arg)) {
                case OSC_EXPR_ARG_TYPE_ATOM:
                    cout << " atom ";
                    break;
                case OSC_EXPR_ARG_TYPE_EXPR:
                {
                    cout << " expr " << osc_expr_rec_getName( osc_expr_getRec(osc_expr_arg_getExpr(arg)));
                    //OdotExpr( osc_expr_arg_getExpr(arg) ).print();
                }
                    break;
                case OSC_EXPR_ARG_TYPE_OSCADDRESS:
                    cout << " osc addr: " << osc_expr_arg_getOSCAddress(arg);
                    break;
                case OSC_EXPR_ARG_TYPE_FUNCTION:
                    cout << " func ";
                    break;
                default:
                    cout << " ?? " << endl;
                    break;
            }
            cout << endl;
            
            arg = osc_expr_arg_next(arg);
        }
        
        e = osc_expr_next(e);
    }
}
