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

// the interchange structure used for OSC bundles
// compatible with odot 'bundle_s' struct
struct odot_bundle
{
    long len;
    char* data;
};

// a rectangle
struct t_rect
{
    std::vector<float> pos;
    float w, h;
};

// type for callbacks to the host environment
typedef void (*symbolistCloseCallback)( void * win);
typedef void (*symbolistUpdateCallback)( void * win, int n );


enum UI_EditType { edit, draw };

#endif

