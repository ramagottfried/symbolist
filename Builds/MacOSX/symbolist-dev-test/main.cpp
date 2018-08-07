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

SymbolistPath path;


vector<SymbolistPoint> parseSegment(string& seg, const char type, SymbolistPoint startPt)
{
    size_t prevnumpos = 0, numpos;
    
    vector<SymbolistPoint> pts;
    double x = 0;
    int count = 0;

    while ( (numpos = seg.find_first_of(", ", prevnumpos) ) != std::string::npos )
    {
        if( count % 2 == 0 )
        {
            x = stod( seg.substr(prevnumpos, numpos-prevnumpos));
        }
        else
        {
            pts.emplace_back(SymbolistPoint(x, stod( seg.substr(prevnumpos, numpos-prevnumpos) ) ) );
        }
        count++;
        prevnumpos = numpos+1;
    }
    
    if (prevnumpos < seg.length() )
    {
        if( count % 2 == 0 )
        {
            cout << "error: uneven number of points" << endl;
            return vector<SymbolistPoint>();
        }
        
        pts.emplace_back(SymbolistPoint(x, stod( seg.substr(prevnumpos, numpos-prevnumpos) ) ) );
    }
    
    
    switch (type) {
        case 'M':
        case 'm':
            return pts;
            break;
        case 'L':
            if( pts.size() == 1 )
            {
                path.addSegment(startPt, pts[0]);
            }
            else
                cout << "L parse error: wrong number of points" << endl;
            break;
        case 'l':
            if( pts.size() == 2 )
            {
                path.addSegment(startPt, startPt+pts[0]);
            }
            else
                cout << "l parse error: wrong number of points" << endl;
            break;
            
        case 'Q':
            if( pts.size() == 2 )
            {
                path.addSegment(startPt, pts[0], pts[1]);
            }
            else
                cout << "Q parse error: wrong number of points" << endl;
            break;
        case 'q':
            if( pts.size() == 2 )
            {
                path.addSegment(startPt, startPt+pts[0], startPt+pts[1]);
            }
            else
                cout << "q parse error: wrong number of points" << endl;
            break;
        
        case 'C':
            if( pts.size() == 3 )
            {
                path.addSegment(startPt, pts[0], pts[1], pts[2]);
            }
            else
                cout << "C parse error: wrong number of points" << endl;
            break;
        case 'c':
            if( pts.size() == 3 )
            {
                path.addSegment(startPt, startPt+pts[0], startPt+pts[1], startPt+pts[2]);
            }
            else
                cout << "c parse error: wrong number of points" << endl;
            break;
        default:
            break;
    }
    
    cout << type << " " << pts.size() << endl;
    for( auto p : pts )
    {
        cout << "x " << p.getX() << " y " << p.getY() << endl;
    }
    cout << "--" << endl;
    
    return pts;
}

int main(int argc, const char * argv[])
{
    
    
    string svg_path = "M0.5,0.12q10,1.1 .20,21Q11,12 22,23";
    
    SymbolistPath p(svg_path);
    p.print();
    
    auto rect = p.getBounds();
    cout << rect.getX() << " " << rect.getY() << " " << rect.getWidth() << " " << rect.getHeight() << endl;
    
    /*
    
    
    
    SymbolistPoint startPt;
    std::size_t prev = 0, pos;
    string seg;
    char type = '\0', nexttype = '\0';
    
    while ( (pos = svg_path.find_first_of("MmLlQqCcZ", prev) ) != std::string::npos )
    {
        nexttype = svg_path[pos];
        
        if (pos > prev)
        {
            seg = svg_path.substr(prev, pos-prev);

            auto pts = parseSegment(seg, type, startPt);
            if( !pts.size() )
            {
                return 1;
            }

            startPt = pts.back();
        
        }
        prev = pos+1;
        type = nexttype;
    }
    
    if (prev < svg_path.length())
    {
        seg = svg_path.substr(prev, std::string::npos);
        auto pts = parseSegment(seg, type, startPt);
        if( !pts.size() )
        {
            return 1;
        }
    }
    */
    return 0;
}












