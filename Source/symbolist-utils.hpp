#ifndef symbolist_utils_hpp
#define symbolist_utils_hpp

#include "JuceHeader.h"
#include <stdio.h>
#include <string>
#include <iostream>
#include <sstream>

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
#define DEBUG_FULL(debugMessage) cout << "** " << __FILENAME__ << " line " << __LINE__ << " [" << __func__ << "]" \
									  << ": " << debugMessage;

/**
 * Prints the debugMessage without file name, line number
 * and scope function infos.
 */
#define DEBUG_INLINE(debugMessage) cout << debugMessage;

/**
 * Prints the current file name, line number, and scope function.
 */
#define DEBUG_TRACE() cout << "** " << __FILENAME__ << " line " << __LINE__ << " [" << __func__ << "]" << endl;

#else

#define DEBUG_FULL(debugMessage)
#define DEBUG_INLINE(debugMessage)
#define DEBUG_TRACE()

#endif

template <typename T>
void printRect( const Rectangle<T> &rect, const String &rectangleName = "rect" )
{
	DEBUG_INLINE(rectangleName << " " << rect.getX() << " " << rect.getY() << " " << rect.getWidth() << " " << rect.getHeight() << std::endl)
}

template <typename T>
void printPoint(Point<T> point, const String &name = "point" )
{
    DEBUG_INLINE(name << " " << point.getX() << " " << point.getY() << "\n")
}

template <typename T>
string pointerToString(T* pointer)
{
	const void * address = static_cast<const void*>(pointer);
	
	stringstream ss;
	ss << address;
	
	return ss.str();
}

#endif



