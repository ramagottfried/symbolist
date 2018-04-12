/************************************************************************************/
/*!
 *  @file       symbolist.hpp
 *  @brief      Library for symbolic score display and editing
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

SYMBOLIST_API void* symbolistNew();
SYMBOLIST_API void symbolistFree(void* symbolist_handler);

SYMBOLIST_API void symbolistOpenWindow(void* symbolist_handler);
SYMBOLIST_API void symbolistCloseWindow(void* symbolist_handler);

SYMBOLIST_API void symbolistRegisterCloseCallback(void* symbolist_handler, symbolistCloseCallback callback);
SYMBOLIST_API void symbolistRegisterUpdateCallback(void* symbolist_handler, symbolistUpdateCallback callback);
SYMBOLIST_API void symbolistRegisterTransportCallback(void* symbolist_handler, symbolistTransportCallback callback);

SYMBOLIST_API void symbolistWindowToFront(void* symbolist_handler);
SYMBOLIST_API void symbolistWindowSetName(void* symbolist_handler, char *name);

SYMBOLIST_API int  symbolistGetNumSymbols(void* symbolist_handler);
SYMBOLIST_API t_osc_bndl_s * symbolistGetSymbol(void* symbolist_handler, int n);
SYMBOLIST_API void symbolistSetOneSymbol(void* symbolist_handler, t_osc_bndl_s *bundle);
SYMBOLIST_API void symbolistSetSymbols(void* symbolist_handler, int n, t_osc_bndl_s **bundle_array);

SYMBOLIST_API int  symbolistGetNumPaletteSymbols(void* symbolist_handler);
SYMBOLIST_API t_osc_bndl_s* symbolistGetPaletteSymbol(void* symbolist_handler, int n);
SYMBOLIST_API void symbolistSetOnePaletteSymbol(void* symbolist_handler, t_osc_bndl_s *bundle);
SYMBOLIST_API void symbolistSetPaletteSymbols(void* symbolist_handler, int n, t_osc_bndl_s **bundle_array);

SYMBOLIST_API void symbolistClearScore(void* symbolist_handler);
SYMBOLIST_API void symbolistSetTime(void* symbolist_handler, float time_ms);

SYMBOLIST_API t_osc_bndl_s* symbolistGetDurationBundle(void* symbolist_handler);

SYMBOLIST_API t_osc_bndl_s* symbolistGetSymbolsAtTime(void* symbolist_handler, float t);
SYMBOLIST_API t_osc_bndl_s* symbolistGetScoreBundle(void* symbolist_handler );


SYMBOLIST_API int symbolistImportSVG( void* symbolist_handler, const char * filename );


#endif /* symbolist_hpp */
