#include "OdotBundle.hpp"
#include "OdotHash.hpp"

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
    
    OdotBundleHash hash(b);
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
            /id : 1,
            /x : 0,
            /y : 10,
            /h : 10,
            /w : 10
        },
        /symbol/2 : {
            /id : 2,
            /x : 0,
            /y : 5,
            /h : 10,
            /w : 10
        },
        /symbol/3 : {
            /id : 3,
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
    
    m_score.print();
    
    OdotBundleHash hash( m_score );
    hash.select("/symbol");
    auto stave_vec = hash.getVector();
    
    OdotMessage compareFn = m_score.getMessage("/stave/sort/fn");
    OdotExpr compareExpr( "/t = /stave/sort/fn( /stave/a, /stave/b )" );
    
    sort( stave_vec.begin(), stave_vec.end(),
         [&compareExpr, &compareFn](OdotBundle& a, OdotBundle& b){
             OdotBundle test(compareFn);
             test.addMessage("/stave/a", a);
             test.addMessage("/stave/b", b);
             test.applyExpr( compareExpr );
             return test.getMessage("/t").getInt();
         });
    
    
    OdotMessage pixTimeFn = m_score.getMessage("/stave/pixTime/fn");
    OdotExpr pixTimeApplyExpr(  R"(
                                     /stave/pixTime/fn( /time ),
                                     delete(/stave/pixTime/fn), delete(/time)
                                )" );
    
    float time = 0.0f;
    for( auto& staff : stave_vec )
    {
        staff.addMessage("/time", time);
        staff.addMessage( pixTimeFn );
        staff.applyExpr( pixTimeApplyExpr );
        
        time = staff.getMessage("/end/time").getFloat();
    }

    for( auto& s : stave_vec )
    {
        s.print();
    }
    
    return 0;
}
