#include "OdotBundle.hpp"
#include "OdotSelect.hpp"

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



int main(int argc, const char * argv[])
{
    
    string symbolstr = R"(
        /symbol/1 : {
            /id : "/symbol/1",
            /name : "foo",
            /type : "staff",
            /x : 0,
            /y : 10,
            /h : 10,
            /w : 10
        },
        /symbol/2 : {
            /id : "/symbol/2",
            /staff/name : "foo",
            /staff/id : "/symbol/1",
            /x : 0,
            /y : 5,
            /h : 10,
            /w : 10
        },
        /symbol/3 : {
            /id : "/symbol/3",
            /x : 0,
            /y : 0,
            /h : 10,
            /w : 10
        }
     )";
    
    OdotBundle m_score( symbolstr );
    
    if( !m_score.addressExists("/stave/sort/fn") )
        m_score.addMessage("/stave/sort/fn",
                        R"(
                            lambda([a,b],
                              (a./y < b./y) && (b./x < (a./x + a./w))
                            )
                        )" );
    
    if( !m_score.addressExists("/stave/pixTime/fn") )
    {
        m_score.addMessage("/stave/pixTime/fn",
                           R"(
                           lambda([prev_time],
                                  /start/time = prev_time,
                                  /end/time = /start/time + (/w * 0.01)
                             )
                           )" );
    }
    
   // m_score.print();
    
    OdotSelect m_symbol_table( m_score );
    m_symbol_table.select("/symbol");
    auto symbol_vec = m_symbol_table.getVector();
    
    OdotSelect m_type_selector( m_score );
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
    OdotExpr pixTimeApplyExpr(  R"(
                                     /stave/pixTime/fn( /time ),
                                     delete(/stave/pixTime/fn), delete(/time)
                                )" );
    
    float time = 0.0f;
    for( auto& staff_msg : stave_vec )
    {
        auto staff = staff_msg.getBundle();
        staff.addMessage("/time", time);
        staff.addMessage( pixTimeFn );
        staff.applyExpr( pixTimeApplyExpr );
        m_score.addMessage( staff_msg.getAddress(), staff );
        
        time = staff.getMessage("/end/time").getFloat();
    }

    m_score.print();
    
    
    for( auto& sym_msg : symbol_vec )
    {
        auto s = sym_msg.getBundle();
        auto staff_id = s.getMessage("/staff/id").getString();
        if( staff_id.size() > 0 )
        {
            auto linked_staff = m_symbol_table.get( "/symbol/1" ).getBundle();
            linked_staff.print();
        }
    }

    
    return 0;
}
