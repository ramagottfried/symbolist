/************************************************************************************/
/*!
 *  @file       symbolist.hpp
 *  @brief      Laibrary for symbolic score display and editing
 *  @author     Jean Bresson & Rama Gottfried
 *  @date       10/05/2017
 *
 */
/************************************************************************************/
#ifndef symbolist_hpp
#define symbolist_hpp

#include "types.h"

#ifdef _MSC_VER
	#define SYMBOLIST_VISIBILITY_DEFAULT	__declspec( dllexport )
#else
	#define SYMBOLIST_VISIBILITY_DEFAULT	__attribute__ ((visibility ("default")))
#endif

#ifdef __cplusplus
#define SYMBOLIST_C_EXPORTS   extern "C"
#else
#define SYMBOLIST_C_EXPORTS
#endif

#define SYMBOLIST_API SYMBOLIST_C_EXPORTS SYMBOLIST_VISIBILITY_DEFAULT

SYMBOLIST_API const char* symbolistInfo();

SYMBOLIST_API void* symbolistNewWindow();
//SYMBOLIST_API void* symbolistNewWindowWithSymbols(int n, odot_bundle **bundle_array);
SYMBOLIST_API void symbolistCloseWindow(void* maincomponent);

SYMBOLIST_API void symbolistRegisterCloseCallback(void* maincomponent, symbolistCloseCallback callback);
SYMBOLIST_API void symbolistRegisterUpdateCallback(void* maincomponent, symbolistUpdateCallback callback);
SYMBOLIST_API void symbolistRegisterTransportCallback(void* maincomponent, symbolistTransportCallback callback);

SYMBOLIST_API void symbolistWindowToFront(void* maincomponent);
SYMBOLIST_API void symbolistWindowSetName(void* maincomponent, char *name);

SYMBOLIST_API int  symbolistGetNumSymbols(void* maincomponent);
SYMBOLIST_API odot_bundle* symbolistGetSymbol(void* maincomponent, int n);
SYMBOLIST_API void symbolistSetSymbols(void* maincomponent, int n, odot_bundle **bundle_array);
SYMBOLIST_API void symbolistSetTime(void* maincomponent, int time_ms);


#endif /* symbolist_hpp */
