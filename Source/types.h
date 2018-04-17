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
#include "osc_bundle_s.h"

// type for callbacks to the host environment
typedef void (*symbolistCloseCallback)( void * win);
typedef void (*symbolistUpdateCallback)( void * win, int n );
typedef void (*symbolistTransportCallback)( void * win, int command );

enum UI_EditType { SELECTION, DRAW };
enum UI_DrawType { FROM_TEMPLATE, FREE_DRAW };

#endif


