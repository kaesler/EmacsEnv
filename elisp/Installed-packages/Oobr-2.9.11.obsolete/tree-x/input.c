/* ----------------------------------------------------------------------------
 * File    : input.c
 * Purpose : input routine to create a Tree from an input file
 * ----------------------------------------------------------------------------
 */

#include <ctype.h>
#include <string.h>
#include "defs.h"
#include "tree.h"
#include "input.h"

char *EnvNm;                 /* Stores name of current Envir file */
static int tokDepth = 0;     /* Depth in tree of current token */
static int prevTokDepth;     /* Depth in tree of prev token */

static void SaveSubtree();

/* ----------------------------------------------------------------------------
 * 
 *   GetNextToken() reads the next token from the file indicated by 'fp' and
 *   returns a token. If the token is TOKEN_LABEL, the lexeme is returned
 *   in 'lexeme'. If memory could not be allocated for 'lexeme', it is NULL.
 * 
 *   The following tokens are supported:
 * 
 *     - TOKEN_LABEL: a string of characters, up to 'TOKEN-MAXSIZ'
 *       characters, delimited by number of leading spaces and newlines.
 *       If a label has more than this number of characters, the rest are
 *       ignored. 
 *     - TOKEN_EOF
 * 
 * ----------------------------------------------------------------------------
 */

int
  GetNextToken(fp, lexeme)
FILE *fp;
char **lexeme;
{
  static   char  lexbuf[INPUT_BUFSIZ];
  register char *curbuf = lexbuf;
  register int   charct = 0;
  register int   c;
  int done = FALSE;
  
  prevTokDepth = tokDepth;
  tokDepth = 0;
  
  c = getc(fp);
  
  /* skip over leading whitespace */
  while (c == ' ')
    {
      tokDepth++;
      c = getc(fp);
    }
  tokDepth /= 2;
  
  while (1)
    {
      switch (c)
	{
	case EOF:
	  return (TOKEN_EOF);
	case '\n':
	  *curbuf = '\0';
	  *lexeme = strdup(lexbuf);
	  return (TOKEN_LABEL);
	  break;
	default:
	  *curbuf++ = c;
	  charct++;
	  /* check for buffer overflow */
	  if (charct >= TOKEN_MAXSIZ)
	    {
	      *curbuf = '\0';
	      *lexeme = strdup(lexbuf);
	      /* since buffer is full, skip over remaining chars */
	      c = getc(fp);
	      while (c != '\n' && c != EOF)
		c = getc(fp);
	      if (c == EOF)
		ungetc(c, fp);
	      return (TOKEN_LABEL);
	    }
	  else
	    c = getc(fp);
	}
    }
}


/* ----------------------------------------------------------------------------
 * 
 *   SetNodeLabelAndValue() sets the label text of the specified node and
 *   stores any string value following the label and preceded by a "^^"
 *   delimiter. 
 * 
 * ----------------------------------------------------------------------------
 */

void
SetNodeLabelAndValue(node, label_and_value)
   Tree *node;
   char *label_and_value;
{
   char*       val;

   if (val = strstr(label_and_value, "^^"))
       {
           /* Set node value to string following ^^ delimiter. */
           node->value = val+2;
           /* Erase value from input string, leaving only label. */
           *val = '\0';
       }
   else
       {   node->value = NULL; }
   SetNodeLabel(node, label_and_value);
}


/* ----------------------------------------------------------------------------
 * 
 *   ReadTreeFromFile() takes a filename argument and constructs
 *   a Tree from the labels in the file. If a tree could be constructed,
 *   even partially, it is returned by the function. NULL is returned if
 *   the file could not be opened or there was insufficient memory for
 *   creating the tree.
 * 
 * ----------------------------------------------------------------------------
 */

Tree*
  ReadTreeFromFile(fname, error)
char *fname;
ErrCode *error;
{
  FILE *infile;
  int   inside_list = 0;	/* for semantic checking */
  int   first_child = TRUE;
  
  int   token;
  char *label;
  
  Tree *tree = NULL;		/* the return value of this function  */
  Tree *parent = NULL;		/* parent of 'node'                   */
  Tree *node;			/* current node                       */
  Tree *new_node;		/* new node to add after current node */
  
  *error = ERR_NONE;
  
  infile = fopen(fname, "r");
  if (infile == NULL)
    {
      *error = ERR_OPENFAIL;
      return (NULL);
    }
  
  /* first line of file is Envir file name, save */
  token = GetNextToken(infile, &label);
  if (token == TOKEN_EOF)
    {
      *error = ERR_EMPTYFILE;
      fclose(infile);
      return (NULL);
    }
  else if (token == TOKEN_LABEL)
    {
      if (label == NULL)
	{
	  *error = ERR_MEMALLOC;
	  fclose(infile);
	  return (NULL);
	}
      EnvNm = strdup(label);
    }
  
  /* set up root node */
  token = GetNextToken(infile, &label);
  if (token == TOKEN_EOF)
    {
      *error = ERR_EMPTYFILE;
      fclose(infile);
      return (NULL);
    }
  else if (token == TOKEN_LABEL)
    {
      if (label == NULL)
	{
	  *error = ERR_MEMALLOC;
	  fclose(infile);
	  return (NULL);
	}
      tree = MakeNode();
      if (tree == NULL)
	{
	  *error = ERR_MEMALLOC;
	  fclose(infile);
	  free(label);
	  return(NULL);
	}
      SetNodeLabelAndValue(tree, label);
      tree->parent = NULL;
      node = tree;
    }
  else
    {
      *error = ERR_NOROOT;
      fclose(infile);
      return (NULL);
    }
  
  /* add children and siblings */
  while (1)
    {
      token = GetNextToken(infile, &label);
      if (token == TOKEN_EOF)
	break;
      
      if (tokDepth > prevTokDepth)  /* then new subtree */
	{
	  inside_list++;
	  first_child = TRUE;
	  parent = node;
	}
      else if (tokDepth < prevTokDepth)  /* then end of subtree */
	if (!inside_list)
	  {
	    *error = ERR_NOBEGIN;
	    fclose(infile);
	    return (tree);
	  }
	else
	  while (tokDepth < inside_list)
	    {
	      inside_list--;
	      node = node->parent;
	      parent = node->parent;
	    }
      
      if (label == NULL)
	{
	  *error = ERR_MEMALLOC;
	  fclose(infile);
	  return (tree);
	}
      if (parent == NULL)
	{
	  *error = ERR_MANYROOT;
	  fclose(infile);
	  free(label);
	  return (tree);
	}
      else
	{
	  new_node = MakeNode();
	  if (new_node == NULL)
	    {
	      *error = ERR_MEMALLOC;
	      fclose(infile);
	      free(label);
	      return (tree);
	    }
	  SetNodeLabelAndValue(new_node, label);
	  new_node->parent = parent;
	  
	  if (first_child)
	    {
	      new_node->parent->child = new_node;
	      first_child = FALSE;
	    }
	  else
	    node->sibling = new_node;
	  
	  node = new_node;
/*
 *	  printf("%3d tok: '%s'; tokDepth: %d; prevTokDepth: %d; inside_list: %d\n",
 *	     NumNodes, node->label.text, tokDepth, prevTokDepth, inside_list);
 */
	}
    }
  fclose(infile);
  return (tree);
}


/* ----------------------------------------------------------------------------
 * 
 *   SaveTreeToFile() takes a tree and saves it to a file specified by 'fname.'
 *   If the file could not be opened for writing, False is returned. Otherwise,
 *   True is returned.
 * 
 * ----------------------------------------------------------------------------
 */

SaveTreeToFile(tree, fname)
     Tree *tree;
     char *fname;
{
  FILE *outfile;
  
  outfile = fopen(fname, "w");
  if (outfile == NULL)
    return (FALSE);
  
  fprintf(outfile, "%s\n", EnvNm);   /* Save Env File Name */
  fprintf(outfile, "%s\n", tree->label.text);
  if (tree->child)
    SaveSubtree(tree->child, 0, outfile);
  
  fclose(outfile);
  return (TRUE);
}


/* ----------------------------------------------------------------------------
 * 
 *   SaveSubtree() is the recursive procedure that supports SaveTreeToFile().
 *
 * ----------------------------------------------------------------------------
 */

static void
  SaveSubtree(tree, level, fp)
Tree *tree;
int level;
FILE *fp;
{
  int i;
  
  level++;
  for ( ; tree ; tree = tree->sibling)
    {
      for (i = 0 ; i < level ; i++)
	{
	  putc(' ', fp);
	  putc(' ', fp);
	}
      fprintf(fp, "%s\n", tree->label.text);
      if (tree->child)
	SaveSubtree(tree->child, level, fp);
    }
  level--;
}


