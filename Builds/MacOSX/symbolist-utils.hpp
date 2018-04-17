#ifndef symbolist_utils_hpp
#define symbolist_utils_hpp

#include <stdio.h>
#include <string>
#include <iostream>

using namespace std;

#ifdef DEBUG

/**
 * Prints a debug message specifying the containing function
 * and the line number at which this macro is called.
 */
#define D_(debugMessage) cout << __func__ << " line " << __LINE__ << ":" << endl << debugMessage << endl;

#else
#define D_(debugMessage);
#endif

#endif