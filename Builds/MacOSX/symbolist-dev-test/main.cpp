#include "OdotBundle.hpp"
#include "OdotSelect.hpp"
#include "TimePointArray.h"

#include <iostream>

#include <sys/time.h>

#include "SymbolistPath.hpp"
#include "SVGEncoder.hpp"

void hashtest()
{
    OdotBundle b;
    
    OdotBundle sub1;
    OdotBundle sub2;
    
    sub1.addMessage("/foo", 1);
    sub2.addMessage("/bar", 2);
    
    b.addMessage("/sub/1", sub1 );
    b.addMessage("/sub/2", sub2 );
    b.addMessage("/ff/1", sub1 );
    b.addMessage("/dd/2", sub2 );
    
    b.print();
    
    OdotSelect hash(b);
    hash.select( OdotMessage("/bar", 2) );
    hash.print();
    auto vec = hash.getVector();
    for( auto o : vec )
    {
        o.print();
    }
    //hash.get("/sub/2").print();
}




void timepointTest()
{
    
    string symbolstr = R"(
    /symbol : {
        /1 : {
            /name : "foo",
            /id : "/1",
            /type : "staff",
            /x : 0,
            /y : 10,
            /h : 10,
            /w : 100
        },
        /2 : {
            /name : "note",
            /id : "/2",
            /staff/name : "foo",
            /staff/id : "/1",
            /x : 1,
            /y : 5,
            /h : 10,
            /w : 10
        },
        /3 : {
            /name : "note",
            /id : "/3",
            /staff/name : "foo",
            /staff/id : "/1",
            /x : 5,
            /y : 0,
            /h : 10,
            /w : 10
        },
        /4 : {
            /name : "note",
            /id : "/4",
            /staff/name : "foo",
            /staff/id : "/1",
            /x : 7,
            /y : 0,
            /h : 10,
            /w : 10
        }
    }
    )";
    
    
    
    OdotBundle m_score( symbolstr );
    
    if( !m_score.addressExists("/stave/sort/fn") )
        m_score.addMessage("/stave/sort/fn",
                           R"~(
                           lambda([a,b],
                                  (a./y < b./y) && (b./x < (a./x + a./w))
                                  )
                           )~" );
        
        if( !m_score.addressExists("/stave/pixTime/fn") )
        {
            m_score.addMessage("/stave/pixTime/fn",
                               R"~(
                               lambda([prev_time],
                                      /time/start = prev_time,
                                      /time/end = /time/start + (/w * 0.01)
                                      )
                               )~" );
        }
    
    if( !m_score.addressExists("/stave/event/pixTime/fn") )
    {
        m_score.addMessage("/stave/event/pixTime/fn",
                           R"~(
                           lambda([stave],
                                  /time/start = stave./time/start + (/x - stave./x) * 0.01 ,
                                  /time/end = /time/start + (/w * 0.01)
                                  )
                           )~" );
    }
    
    if( !m_score.addressExists("/stave/event/timePix/fn") )
    {
        m_score.addMessage("/stave/event/timePix/fn",
                           R"~(
                           lambda([stave],
                                  /x = stave./x + ( (/time/start - stave./time/start) * 100. ),
                                  /w = (/time/end - /time/start) * 100.
                                  )
                           )~" );
    }
    
    // m_score.print();
    
    OdotBundle symbols = m_score.getMessage("/symbol").getBundle();
    
    OdotSelect sym_select(symbols);
    sym_select.select();
    auto symbol_vec = sym_select.getVector();
    
    OdotSelect m_type_selector( symbols );
    m_type_selector.select( OdotMessage("/type", "staff") ); // how do we know that this is bundles now, not the messages?
    auto stave_vec = m_type_selector.getVector();
    m_type_selector.print();
    
    OdotMessage compareFn = m_score.getMessage("/stave/sort/fn");
    OdotExpr compareExpr( "/t = /stave/sort/fn( /stave/a, /stave/b )" );
    
    sort( stave_vec.begin(), stave_vec.end(),
         [&compareExpr, &compareFn](OdotMessage& a, OdotMessage& b){
             OdotBundle test(compareFn);
             test.addMessage("/stave/a", a.getBundle() );
             test.addMessage("/stave/b", b.getBundle() );
             test.applyExpr( compareExpr );
             return test.getMessage("/t").getInt();
         });
    
    
    OdotMessage pixTimeFn = m_score.getMessage("/stave/pixTime/fn");
    OdotExpr pixTimeApplyExpr(  R"~(
                              /stave/pixTime/fn( /time ),
                              delete(/stave/pixTime/fn), delete(/time)
                              )~" );
    
    float time = 0.0f;
    for( auto& staff_msg : stave_vec )
    {
        auto staff = staff_msg.getBundle();
        staff.addMessage("/time", time);
        staff.addMessage( pixTimeFn );
        staff.applyExpr( pixTimeApplyExpr );
        symbols.addMessage( staff_msg.getAddress(), staff );
        
        time = staff.getMessage("/end/time").getFloat();
    }
    
    OdotMessage eventPixTimeFn = m_score.getMessage("/stave/event/pixTime/fn");
    
    eventPixTimeFn.print();
    
    OdotExpr eventPixTimeApplyExpr(  R"~(
                                   /stave/event/pixTime/fn( /stave ),
                                   delete(/stave/event/pixTime/fn), delete(/stave)
                                   )~" );
    
    TimePointArray m_time_points;
    
    for( auto& sym_msg : symbol_vec )
    {
        auto sym = sym_msg.getBundle();
        auto staff_id = sym.getMessage("/staff/id").getString();
        if( staff_id.size() > 0 )
        {
            auto linked_staff = sym_select[staff_id].getBundle();
            if( linked_staff.size() > 0 )
            {
                sym.addMessage("/stave", linked_staff );
                sym.addMessage( eventPixTimeFn );
                sym.applyExpr( eventPixTimeApplyExpr );
                symbols.addMessage(sym_msg.getAddress(), sym );
                m_time_points.addSymbol( sym, linked_staff );
            }
        }
    }
    
    m_score.addMessage("/symbol", symbols);
    
    m_time_points.printTimePoints();
    
    auto b = m_time_points.getSymbolsAtTime(0.02);
    b.print();
    b = m_time_points.getSymbolsAtTime(0.2);
    b.print();
    
    
}


void timepointTest2()
{
    
    /*
        use lower_bound to find reference stave for non-time based event mapping lookups (e.g. y axis, etc.)
     
        * assume that a score contains /time'd bundles *
     
        palette symbols will be in the order they are listed, that way if a new one is assigned it will overwrite the previous one.
            no two symbols should have the same name
     
     actually the system maybe should hold the time information, since there could be multiple staves, and the staff/clef mapping could change mid-system
     
     ... but then what about the issue of Clefs?
        for now let's do one stave per system and not change stave mid-system
     
     */
    
    
    string symbolstr = R"bundle(
    /layout : {
        /page : {
            /margins : {
                /left : 10,
                /right : 10,
                /top : 10,
                /bottom : 10
            }
        },
        /system : {
            /margin/topbottom : [20, 20],
            /staves : {
                /piccolo : {
                    /margin : [10, 10],
                    /name : "piccolo"
                },
                /oboe : {
                    /margin : [10, 10],
                    /name : "oboe"
                },
            }
        }
    },
    /palette : {
        /foo : {
            /type : "stave",
            /sort : "lambda([a, b],
                (a./y < b./y) && (b./x < (a./x + a./w))
            )",
            /pixTime : "lambda([t],
                /time/start = t,
                /time/end = t + (/w * 0.01)
            )",
            /event/pixTime : "lambda([stave],
                /time/start = stave./time/start + (/x - stave./x) * 0.01 ,
                /time/end = /time/start + (/w * 0.01)
            )",
            /event/timePix : "lambda([stave],
                /x = stave./x + ( (/time/start - stave./time/start) * 100. ),
                /w = (/time/end - /time/start) * 100.
            )"
        }
    },
    /symbol : {
        /1 : {
            /name : "foo",
            /type : "stave",
            /time/start : 0,
            /time/end : 10
            .... add graphic info here ...
        },
        /2 : {
            /name : "foo",
            /type : "stave",
            /x : 0,
            /y : 20,
            /h : 10,
            /w : 100
            /time/start :
        },
        /3 : {
            /name : "foo",
            /type : "stave",
            /x : 0,
            /y : 30,
            /h : 10,
            /w : 100
        },
        /4 : {
            /name : "note",
            /stave : "foo",
            /x : 1,
            /y : 5,
            /h : 10,
            /w : 10
        },
        /5 : {
            /name : "note",
            /stave : "foo",
            /x : 5,
            /y : 0,
            /h : 10,
            /w : 10
        },
        /6 : {
            /name : "note",
            /stave : "foo",
            /x : 7,
            /y : 0,
            /h : 10,
            /w : 10
        }
    }
    )bundle";
    
    OdotBundle m_score( symbolstr );
   
    // m_score.print();
    
    OdotBundle symbols = m_score.getMessage("/symbol").getBundle();
    
    OdotSelect sym_select(symbols);
    sym_select.select();
    auto symbol_vec = sym_select.getVector();
    
    OdotSelect m_type_selector( symbols );
    m_type_selector.select( OdotMessage("/type", "staff") ); // how do we know that this is bundles now, not the messages?
    auto stave_vec = m_type_selector.getVector();
    m_type_selector.print();
    
    OdotMessage compareFn = m_score.getMessage("/stave/sort/fn");
    OdotExpr compareExpr( "/t = /stave/sort/fn( /stave/a, /stave/b )" );
    
    sort( stave_vec.begin(), stave_vec.end(),
         [&compareExpr, &compareFn](OdotMessage& a, OdotMessage& b){
             OdotBundle test(compareFn);
             test.addMessage("/stave/a", a.getBundle() );
             test.addMessage("/stave/b", b.getBundle() );
             test.applyExpr( compareExpr );
             return test.getMessage("/t").getInt();
         });
    
    
    OdotMessage pixTimeFn = m_score.getMessage("/stave/pixTime/fn");
    OdotExpr pixTimeApplyExpr(  R"~(
                              /stave/pixTime/fn( /time ),
                              delete(/stave/pixTime/fn), delete(/time)
                              )~" );
    
    float time = 0.0f;
    for( auto& staff_msg : stave_vec )
    {
        auto staff = staff_msg.getBundle();
        staff.addMessage("/time", time);
        staff.addMessage( pixTimeFn );
        staff.applyExpr( pixTimeApplyExpr );
        symbols.addMessage( staff_msg.getAddress(), staff );
        
        time = staff.getMessage("/end/time").getFloat();
    }
    
    OdotMessage eventPixTimeFn = m_score.getMessage("/stave/event/pixTime/fn");
    
    eventPixTimeFn.print();
    
    OdotExpr eventPixTimeApplyExpr(  R"~(
                                   /stave/event/pixTime/fn( /stave ),
                                   delete(/stave/event/pixTime/fn), delete(/stave)
                                   )~" );
    
    TimePointArray m_time_points;
    
    for( auto& sym_msg : symbol_vec )
    {
        auto sym = sym_msg.getBundle();
        auto staff_id = sym.getMessage("/staff/id").getString();
        if( staff_id.size() > 0 )
        {
            auto linked_staff = sym_select[staff_id].getBundle();
            if( linked_staff.size() > 0 )
            {
                sym.addMessage("/stave", linked_staff );
                sym.addMessage( eventPixTimeFn );
                sym.applyExpr( eventPixTimeApplyExpr );
                symbols.addMessage(sym_msg.getAddress(), sym );
                m_time_points.addSymbol( sym, linked_staff );
            }
        }
    }
    
    m_score.addMessage("/symbol", symbols);
    
    m_time_points.printTimePoints();
    
    auto b = m_time_points.getSymbolsAtTime(0.02);
    b.print();
    b = m_time_points.getSymbolsAtTime(0.2);
    b.print();
    
    
}

void readFile()
{
    
    OdotBundle b;
    b.setFromFile("default-score.odot");
    b.print();
}

typedef unsigned long long timestamp_t;

static timestamp_t
get_timestamp ()
{
    struct timeval now;
    gettimeofday (&now, NULL);
    return  now.tv_usec + (timestamp_t)now.tv_sec * 1000000;
}

inline double
get_timedelta(timestamp_t start, timestamp_t end)
{
    return (end - start) / 1000000.0L;
}

void subbundletests()
{
    
    //    vector<OdotBundle> staves;
    
    OdotBundle b;
    b.addMessage("/foo", OdotBundle("/bar", OdotBundle("/steve", 4)));
    //    b.applyExpr("/foo./bar./steve = 111, /foo./bar./steve = /foo");
    
    /*
     timestamp_t t0 = get_timestamp();
     b.applyExpr("/a = /foo, /x./y./z = 100, /q = /foo./bar");
     b.applyExpr("/foo./bar./steve = 111");
     timestamp_t t1 = get_timestamp();
     
     cout << get_timedelta(t0, t1) << endl;
     */
    
    OdotBundle_s s = b.serialize();
    
    // testfunction assignment
    s.applyExpr("/d./b./a./q = quote(lambda([v,x], /z = v, /zz = x)), /d./b./a./q(11,22)" );
   // s.print();
    
    /*
     /a./c./e = 1,
     
     /fn = "lambda([b], /zz = b./c./e)",
     
     /fn( /a ),
     */

     OdotBundle bb;
     bb.applyExpr(R"expr(
     /a./b./c = "lambda([], /z = 1)",
     
     /a./b./c(),
                  
    /a./c./e = 111,

    /fn = "lambda([b], /zz = b./c./e)",

    /fn( /a )
     
     )expr" );
    
     bb.print();
}

void addSub()
{
    string bundlestr = R"bundle(
    /layout : {
        /page : {
            /margins : {
                /left : 10,
                /right : 10,
                /top : 10,
                /bottom : 10
            }
        }
    })bundle";
    
    OdotBundle b(bundlestr);
    
    // need something like assign to bundle member here...
    b.addMessage("/layout./page./margins./foo", 0);
    b.print();
}

void SymbolistUtil_setBounds(OdotBundle& b, double x, double y, double w, double h)
{
    b.addMessage("/x", x);
    b.addMessage("/y", y);
    b.addMessage("/w", w);
    b.addMessage("/h", h);
}

/**
 *  Setting the score from a file
 *
 */
void scoreDev()
{

    OdotBundle file, m_score;
    file.setFromFile("/Users/rama/Documents/symbolist/testscore2_.osc");

    m_score = file;
    
    auto defs = file.getMessage("/defs").getBundle();
    
    auto stave_defs = defs.getMessage("/stave").getBundle().getMessageArray();
    
    for( auto& stv_msg : stave_defs )
    {
        auto stv_name = stv_msg.getAddress();
        auto stv_bndl = stv_msg.getBundle();
        auto stv_graphic = stv_bndl.getMessage("/graphic").getBundle();
       
        // need to scale bounds to fit into palette
        cout << SVGEncoder::graphicObjectToJUCE(stv_graphic) << endl;
        
        auto palette_bndl = stv_bndl.getMessage("/palette").getBundle();
        
        for( auto& palette_msg : palette_bndl.getMessageArray() )
        {
            auto pal_sym_name = palette_msg.getAddress();
            auto pal_sym_bndl = palette_msg.getBundle();
            auto pal_sym_graphic = pal_sym_bndl.getMessage("/graphic").getBundle();

            cout << SVGEncoder::graphicObjectToJUCE(pal_sym_graphic) << endl;

        }
            
        
        
        
    }
    
    
    return;
    
    
    
    auto pages = file.getMessage("/score./page").getBundle().getMessageArray();

    // do system sorting and time assignment before iterating to map time to sub elements
    // pages have no time information, so they are always in the order they are listed.
    
    //1. collect systems
    vector<OdotMessage> systems;
    for( auto& p_msg : pages )
    {
        auto vec = p_msg.getBundle().getMessage("/system").getBundle().getMessageArray();
        systems.reserve( systems.size() + vec.size() );
        systems.insert( systems.end(), vec.begin(), vec.end() );
    }

    //2. all systems have the same base time scaling and sorting function, stored in the /basis bundle.
    //      this can be warped, but there must be a constant basis value.
    
    OdotMessage compareFn = defs.getMessage("/system./time/sort");
    
    OdotExpr compareExpr( "/t = /time/sort( /system/a, /system/b )" );
    
    sort( systems.begin(), systems.end(),
         [&compareExpr, &compareFn](OdotMessage& a, OdotMessage& b){
             OdotBundle test(compareFn);
             test.addMessage("/system/a", a.getBundle() );
             test.addMessage("/system/b", b.getBundle() );
             test.applyExpr( compareExpr );
             return test.getMessage("/t").getInt();
         });

    // <<<< reset system numbering here after ordering?
    
    
    //3. the placement of staves on pages is determined by the /layout
    //      when the system is larger than the page, it needs to be placed on a new page
    //      or, if no page dimensions have been set, just increase the size of the page
    
    // objects in the palette have a default bounds, which describes the scaling?
    // maybe not worth it, since the scaling happens based on the transform...
    // when parsing file, we need to get the bounds for the graphic path
    
    // then iterating through all elements, using time of parent to set children
    OdotBundle context;
    context.addMessage("/defs", defs);
    
    // for now, all systems use the same object def
    auto page_def = defs.getMessage("/page").getBundle();
    auto system_def = defs.getMessage("/system").getBundle();
    
    double page_w = page_def.getMessage("/bounds./w").getFloat();
    double page_h = page_def.getMessage("/bounds./h").getFloat();

    // auto timePixScale = system_def.getMessage("/timePixScale").getFloat();
    // auto timeAxis = system_def.getMessage("/timeAxis").getString();
    
    size_t page_count = 0;
    for( auto& page_msg : pages )
    {
        const string page_addr = "/score./page./" + to_string(page_count+1);
        
        auto page = page_msg.getBundle();
        
        // set page position and size here
        // or if the page should expand, then set size after iterating the page items
        SymbolistRect pageBounds;
        pageBounds.setBounds( page_w * page_count, page_h * page_count, page_w, page_h );
        
        page.addMessage("/bounds", pageBounds.getBundle() );

        // set page in context for child objects
        context.addMessage("/page", page );

        SymbolistRect prevSystemBounds(0,0,0,0);
        
        auto systems = page.getMessage("/system").getBundle().getMessageArray();
        
        size_t system_count = 0;
        for( auto& sys_msg : systems )
        {
            const string system_addr = "/system./"+to_string(system_count+1);

            auto system = sys_msg.getBundle();

//            const string& system_name = system.getMessage("/use").getString();
            auto system_def = defs.getMessage("/system").getBundle();
            auto system_set_script = system_def.getMessage("/script./set/fromOSC");
            
            system.addMessage("/prevBounds", prevSystemBounds.getBundle() );
            system.addMessage("/context", context);
            system.addMessage(system_set_script);
            
            if( system.applyExpr("/set/fromOSC(/context, /prevBounds), delete(/prevBounds), delete(/context), delete(/set/fromOSC)") )
            {
                cout << "error in /system script" << endl;
                return;
            }
            
            SymbolistRect systemBounds( system.getMessage("/bounds").getBundle() );

            // set system in context for child objects
            context.addMessage("/system", system );
            
            SymbolistRect prevStaveBounds(0,0,0,0);

            auto staves = system.getMessage("/stave").getBundle().getMessageArray();
            size_t stave_count = 0;
            for( auto& stave_msg : staves )
            {
                const string stave_addr = "/stave./"+to_string(stave_count+1);

                // set Stave position here

                auto stave = stave_msg.getBundle();
                if( !stave.size() )
                {
                    cout << "parse error: stave message \"" << stave_msg.getAddress() << "\" in stave number " << stave_count+1 << " is not a bundle" << endl;
                    return;
                }
                
                // adjust stave bounds and scale based on script time mapping
                
                const string& stave_name = stave.getMessage("/use").getString();
                auto stave_def = defs.getMessage("/stave."+stave_name).getBundle();
                if( !stave_def.size() )
                {
                    cout << "parse error: no stave definition found for \"" << stave_name << "\"" << endl;
                    return;
                }
                
                auto stave_setFromOSC_msg = stave_def.getMessage("/script./set/fromOSC");
                if( !stave_setFromOSC_msg.size() )
                {
                    cout << "parse error: stave script /set/fromOSC not found in def \"" << stave_name << "\"" << endl;
                    return;
                }
                
                stave.addMessage("/prevBounds", prevStaveBounds.getBundle() );
                stave.addMessage("/context", context);
                stave.addMessage(stave_setFromOSC_msg);

                if( stave.applyExpr("/set/fromOSC(/context, /prevBounds), delete(/context), delete(/set/fromOSC), delete(/prevBounds)") )
                {
                    cout << "error in /stave script" << endl;
                    return;
                }

                SymbolistRect staveBounds( stave.getMessage("/bounds").getBundle() );
                
                SymbolistRect prevSymbolBounds(0,0,0,0);
                
                context.addMessage("/stave", stave );

                auto symbols = stave.getMessage("/symbol").getBundle().getMessageArray();
                size_t symbol_count = 0;
                for( auto& sym_msg : symbols )
                {
                    const string sym_addr = "/symbol./"+to_string(symbol_count+1);

                    auto symbol = sym_msg.getBundle();
                    const string symbol_name = symbol.getMessage("/use").getString();
                    
                    // set Symbol position and size here
                    
                    auto symbol_def = defs.getMessage( "/stave." + stave_name + "./palette." + symbol_name ).getBundle();
                    if( !symbol_def.size() )
                    {
                        cout << "parse error: no stave definition found for \"" << symbol_name << "\"" << endl;
                        return;
                    }
                    
                    
                    auto symbol_path_str = symbol_def.getMessage("/graphic./d").getString();
                    // add error check
                    
                    SymbolistPath symbol_path(symbol_path_str);
                    symbol_path.getBounds();
                    context.addMessage("/pathbounds", symbol_path.getBounds().getBundle() );
                    
                    
                    auto symbol_setFromOSC_msg = symbol_def.getMessage("/script./set/fromOSC");
                    if( !symbol_setFromOSC_msg.size() )
                    {
                        cout << "parse error: stave script /set/fromOSC not found in def \"" << symbol_name << "\"" << endl;
                        return;
                    }
                    
                    symbol.addMessage("/context", context);
                    symbol.addMessage(symbol_setFromOSC_msg);
                    
                    if( symbol.applyExpr("/set/fromOSC(/context), delete(/context), delete(/set/fromOSC)") )
                    {
                        cout << "error in /stave script (" << stave_addr << ")" << endl;
                        return;
                    }
                    
                    cout << sym_addr << endl;
                    stave.addMessage(sym_addr, symbol); // update object at address
                    
                    SymbolistRect symbolBounds( symbol.getMessage("/bounds").getBundle() );
                    prevSymbolBounds = symbolBounds;
                    symbol_count++;
                    
                    staveBounds.expandToFit( symbolBounds );
                }
                
                // set Stave size here based on total symbol size
                stave.addMessage("/bounds", staveBounds.getBundle() );  // << or do this with the resize() function script?

                cout << "adding " << stave_addr << endl;
                system.addMessage(stave_addr, stave); // update object at address
                
                prevStaveBounds = staveBounds;
                stave_count++;

                systemBounds.expandToFit( staveBounds );

            }

            // set System size here based on total symbol size
            system.addMessage("/bounds", systemBounds.getBundle() );

            page.addMessage(system_addr, system); // update object at address
            
            system_count++;
            prevSystemBounds = systemBounds;
        }
        
        
        cout << "adding " << page_addr << endl;
        m_score.addMessage(page_addr, page); // update object at address
        page_count++;
    }
    
    m_score.print();
    m_score.writeToFile("/Users/rama/Documents/symbolist/testscore2_.osc");
    
}

int main(int argc, const char * argv[])
{
    //subbundletests();

    // OdotBundle b;
    // b.applyExpr("/foo./x./y = 1, /z = /foo./x");
    
    
    scoreDev();
    
    /*
    string svg_path = "M104.7-0.7C119.3,53.7-19.8-118.2,2.2,201.8H127c79.7-22.7,156-46.9,202-84c9.2-36.7,6.8-77.2,0-119Z";
    //string svg_path = "M104.7-0.7C119.3,53.7-19.8-118.2,2.2,201.8,119.3,53.7-19.8-118.2,2.2,201.8";

    SymbolistPath p(svg_path);
    p.print();
    */
    
   // cout << x << endl;
    
    return 0;
}












