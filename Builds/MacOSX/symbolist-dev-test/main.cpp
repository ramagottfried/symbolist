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
    
    b.print();
    
    OdotBundleHash hash(b);
    
    hash.get("/sub/1").print();
    
    return 0;
}
