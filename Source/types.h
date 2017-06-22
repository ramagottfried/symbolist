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

// type for callbacks to the host environment
typedef void (*symbolistCloseCallback)( void * win);
typedef void (*symbolistUpdateCallback)( void * win, int n );
typedef void (*symbolistTransportCallback)( void * win, int command );

enum UI_EditType { selection, draw };
enum UI_DrawType { from_template, free_draw };

#endif

