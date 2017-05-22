//
//  types.h
//  symbolist
//
//  Created by Jean Bresson on 14/05/2017.
//
//

#ifndef types_h
#define types_h

#include <vector>

struct odot_bundle
{
    long len;
    char* data;
};

struct t_rect
{
    std::vector<float> pos;
    float w, h;
};


typedef void (*symbolistCloseCallback)( void * win);
typedef void (*symbolistUpdateCallback)( void * win, int n );


#endif

