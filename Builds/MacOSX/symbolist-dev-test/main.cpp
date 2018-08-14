#include "OdotBundle.hpp"
#include "OdotSelect.hpp"
#include "TimePointArray.h"

#include <iostream>

#include <sys/time.h>

#include "SymbolistPath.hpp"


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
    s.print();
    
    
    /*
     /a./c./e = 1,
     
     /fn = "lambda([b], /zz = b./c./e)",
     
     /fn( /a ),
     */
    
    /*
     OdotBundle bb;
     bb.applyExpr(R"expr(
     /a./b./c = "lambda([], /z = 1)",
     
     
     /a./b./c()
     
     )expr" );
     
     
     bb.print();
     */
}


void scoreDev()
{
    OdotBundle m_score;
    m_score.setFromFile("/Users/rama/Documents/symbolist/testscore.osc");

    auto basis = m_score.getMessage("/basis").getBundle();
    auto palette = m_score.getMessage("/palette").getBundle();
    auto pages = m_score.getMessage("/score./page").getBundle().getMessageArray();
   
    // do system sorting and time assignment before iterating to map time to sub elements
    
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
    
    OdotMessage compareFn = basis.getMessage("/time/sort");
    compareFn.print();
    
    OdotExpr compareExpr( "/t = /time/sort( /system/a, /system/b )" );
    
    sort( systems.begin(), systems.end(),
         [&compareExpr, &compareFn](OdotMessage& a, OdotMessage& b){
             OdotBundle test(compareFn);
             test.addMessage("/system/a", a.getBundle() );
             test.addMessage("/system/b", b.getBundle() );
             test.applyExpr( compareExpr );
             test.print();
             return test.getMessage("/t").getInt();
         });
    
    
    //3. the placement of staves on pages is determined by the /layout
    //      when the system is larger than the page, it needs to be placed on a new page
    
    // then iterating through all elements, using time of parent to set children
    for( auto& p_msg : pages )
    {
        auto systems = p_msg.getBundle().getMessage("/system").getBundle().getMessageArray();
     
        // system needs bounds, but to generate the bounds, we need to know how the time aspect fits on the page
        
        for( auto& sys_msg : systems )
        {
            auto staves = sys_msg.getBundle().getMessage("/stave").getBundle().getMessageArray();

            for( auto& stave_msg : staves )
            {
               auto symbols = stave_msg.getBundle().getMessage("/symbol").getBundle().getMessageArray();
                
                for( auto& sym_msg : symbols )
                {
                    auto s = sym_msg.getBundle();
                    s.print();
                    
                }
                
            }

            
            
        }
        
    }
    
    return;
    
    
    
    auto system = m_score.getMessage("/score./page./1./system./1").getBundle();

    
    
    auto staves = system.getMessage("/stave").getBundle();

    for( auto stave_m : staves.getMessageArray() )
    {
        auto stave = stave_m.getBundle();
        
        const string& stave_name = stave.getMessage("/name").getString();
        
        auto stave_prototype = palette.getMessage(stave_name).getBundle();
        double stave_timeConst = stave_prototype.getMessage("/param./timePixScale").getFloat();
        
        auto stave_scripts = stave_prototype.getMessage("/script").getBundle();
        stave_scripts.print();
        
        auto stave_palette = stave_prototype.getMessage("/palette").getBundle();
        
        auto syms = stave.getMessage("/symbol").getBundle();
        
        stave.removeMessage("/symbol");
        
        for ( auto sym : syms.getMessageArray() )
        {
            auto s = sym.getBundle();
            
            const string& s_name = s.getMessage("/name").getString();

            auto sym_prototype = stave_palette.getMessage(s_name).getBundle();
            
            s.addMessage("/script", sym_prototype.getMessage("/script./set/fromOSC").getString() );
            
            s.addMessage( "/stave", stave );
            
            s.print();
            
            
           // s.applyExpr(sym_set_script);
            
            //cout << sym.get_o_ptr() << " " << s_name << "\n\t" << sym_set_script << endl;
            
        //    const string set_bounds_
            
  //          s.applyExpr(sym_set_script)
            
        }
        
    }
    
}

int main(int argc, const char * argv[])
{
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












