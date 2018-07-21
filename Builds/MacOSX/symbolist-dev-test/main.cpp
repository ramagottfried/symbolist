#include "OdotBundle.hpp"
#include "OdotSelect.hpp"
#include "TimePointArray.h"

#include <iostream>


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


int main(int argc, const char * argv[])
{
    
    string symbolstr2 = R"(
        /symbol/1 : {
            /name : "foo",
            /id : "/symbol/1",
            /type : "staff",
            /x : 0,
            /y : 10,
            /h : 10,
            /w : 100
        },
        /symbol/2 : {
            /name : "note",
            /id : "/symbol/2",
            /staff/name : "foo",
            /staff/id : "/symbol/1",
            /x : 1,
            /y : 5,
            /h : 10,
            /w : 10
        },
        /symbol/3 : {
            /name : "note",
            /id : "/symbol/3",
            /staff/name : "foo",
            /staff/id : "/symbol/1",
            /x : 5,
            /y : 0,
            /h : 10,
            /w : 10
        },
        /symbol/4 : {
            /name : "note",
            /id : "/symbol/4",
            /staff/name : "foo",
            /staff/id : "/symbol/1",
            /x : 7,
            /y : 0,
            /h : 10,
            /w : 10
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
    
    OdotBundle m_symbols = m_score.getMessage("/symbol").getBundle();
    m_symbols.selector().select();
    
    auto symbol_vec = m_symbols.selector().getVector();
    
    OdotSelect m_type_selector( m_symbols );
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
        m_symbols.addMessage( staff_msg.getAddress(), staff );
        
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
            auto linked_staff = m_symbols.selector()[staff_id].getBundle();
            if( linked_staff.size() > 0 )
            {
                sym.addMessage("/stave", linked_staff );
                sym.addMessage( eventPixTimeFn );
                sym.applyExpr( eventPixTimeApplyExpr );
                m_symbols.addMessage(sym_msg.getAddress(), sym );
                m_time_points.addSymbol( sym, linked_staff );
            }
        }
    }
    
    m_score.addMessage("/symbol", m_symbols);
    
    m_time_points.printTimePoints();
    
    auto b = m_time_points.getSymbolsAtTime(0.02);
    b.print();
    b = m_time_points.getSymbolsAtTime(0.2);
    b.print();
    
    return 0;
}
