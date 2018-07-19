#include "OdotBundle.hpp"
#include "OdotHash.hpp"

#include <iostream>

int main(int argc, const char * argv[])
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
    
    OdotBundleHash hash(b, "/sub" );
    // hash.print();
    hash.get("/sub/2").print();
    
    return 0;
}
