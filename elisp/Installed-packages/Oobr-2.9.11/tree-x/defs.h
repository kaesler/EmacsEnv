/* ----------------------------------------------------------------------------
 * File    : defs.h
 * Purpose : general defines for dynamic tree program
 * ----------------------------------------------------------------------------
 */

#include <stdio.h>

#define ASSERT(x, msg) \
   if ((x) == NULL) { fprintf(stderr, "%s: %s\n", ProgramName, msg); exit(0); }

#define NASSERT(x, msg) \
   if ((x) == NULL) { fprintf(stderr, "%s: %s\n", ProgramName, msg); \
		      return (NULL); }

#define WARN(msg) \
   fprintf(stderr, "%s: %s\n", ProgramName, msg)

#define WARN_1(msg, arg1) \
   fprintf(stderr, "%s: %s %s", ProgramName, msg, arg1)

#define WARN_4(msg1, arg1, msg2, arg2) \
    fprintf(stderr, "%s: %s %s; %s %s\n", ProgramName, msg1, arg1, msg2, arg2)

#ifdef GLOBALS
#define Global
#else
#define Global extern
#endif

Global char        *ProgramName;

#undef Global
#undef GLOBALS

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef NULL
#define NULL 0L
#endif

#ifndef MAX
#define MAX(x, y) ((x) < (y)) ? (y) : (x)
#endif
