/* ----------------------------------------------------------------------------
 * File    : input.h
 * Purpose : header file for input.c
 * ----------------------------------------------------------------------------
 */

#define TOKEN_MAXSIZ          1023             /* maximum size of token      */
#define INPUT_BUFSIZ          TOKEN_MAXSIZ + 1 /* allow for terminating null */
/*
 *  #define DELIMITER_BEGIN_LIST '{'
 *  #define DELIMITER_END_LIST   '}'
 */ 

/* Possible token types in file */

enum { TOKEN_LABEL,
/*
 *     TOKEN_BEGIN_LIST,
 *     TOKEN_END_LIST,
 */
       TOKEN_EOF };


typedef enum { 
   ERR_OPENFAIL,
   ERR_EMPTYFILE,
   ERR_MEMALLOC,
   ERR_NOBEGIN,
   ERR_NOEND,
   ERR_NOROOT,
   ERR_MANYROOT,
   ERR_NONE,
} ErrCode;

#define NUM_ERRS             7	/* don't count ERR_NONE */


