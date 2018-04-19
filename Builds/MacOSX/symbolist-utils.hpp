#ifndef symbolist_utils_hpp
#define symbolist_utils_hpp

#include <stdio.h>
#include <string>
#include <iostream>

using namespace std;

#ifdef DEBUG

/**
 * Retrieves the current "simple" file name. Contrarily to __FILE__
 * which retrieves the "full path" filename.
 */
#define __FILENAME__ (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)

/**
 * Prints the debug message specifying the current file name
 * line number, and scope function.
 */
#define DEBUG_FULL(debugMessage) (cout << "** " << __FILENAME__ << " line " << __LINE__ << " [" << __func__ << "]" \
									   << ": " << debugMessage)

/**
 * Prints the debugMessage without file name, line number
 * and scope function infos.
 */
#define DEBUG_INLINE(debugMessage) (cout << debugMessage)

/**
 * Prints the current file name, line number, and scope function.
 */
#define DEBUG_TRACE() (cout << "** " << __FILENAME__ << " line " << __LINE__ << " [" << __func__ << "]" << endl)


#else

#define DEBUG_FULL(debugMessage)
#define DEBUG_INLINE(debugMessage)

#endif

#endif
