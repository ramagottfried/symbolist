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


SYMBOLIST_API int symbolistInit();
SYMBOLIST_API int symbolistExit();

SYMBOLIST_API const char* symbolistInfo();

SYMBOLIST_API void *symbolistTest();


#endif /* symbolist_hpp */
