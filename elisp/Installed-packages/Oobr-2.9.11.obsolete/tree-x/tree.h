/* ----------------------------------------------------------------------------
 * File    : tree.h
 * Purpose : Header file for dynamic tree program
 * ----------------------------------------------------------------------------
 */

#define INTF 1		/* enable interface-specific code */

#define FOREACH_CHILD(child, tree) \
   for ((child) = (tree)->child ; (child) ; (child) = (child)->sibling)

#define PT_IN_RECT(p1, p2, x1, y1, x2, y2)   \
	 ((p1) > (x1) &&                      \
	  (p2) > (y1) &&                       \
	  (p1) < (x2) &&                        \
	  (p2) < (y2))

#define PT_IN_EXTENT(p1, p2, extent)                 \
	 ((p1) > (extent).pos.x &&                    \
	  (p2) > (extent).pos.y &&                     \
	  (p1) < ((extent).pos.x + (extent).width) &&   \
	  (p2) < ((extent).pos.y + (extent).height))

#define IS_LEAF(node) \
   ((node)->child == NULL)

typedef struct line  Polyline;
typedef struct tnode Tree;

typedef struct point {
   int x;
   int y;
} Point;

typedef struct extent {
   Point pos;			/* top left corner of rectangle     */
   int width;
   int height;
} Extent;

struct line {
   int dx, dy;
   Polyline *link;
};

typedef struct polygon {
   struct {
      Polyline *head;
      Polyline *tail;
   } lower, upper;
} Polygon;

typedef struct label {
   char *text;			/* the actual label text */
   int len;			/* length of label text  */
   int xoffset;			/* the X offset of label inside rectangle */
   int yoffset;			/* the Y offset of label inside rectangle */
} Label;

struct tnode {
   Tree    *parent;
   Tree    *child;
   Tree    *sibling;
   int      width;
   int      height;
   int      border;
   Polygon  contour;
   Point    offset;		/* offset is relative to 'predecessor'    */
   Point    pos;	        /* position is screen coordinates of node */
   Point    old_pos;	        /* position is screen coordinates of node */
   int      node_height;	/* height of node in tree */
   /* all fields below are interface-specific */
   Label    label;
   char*    value;
   Extent   subextent;		/* extent of subtree (excluding this node) */
   Polygon  old_contour;	/* for caching old contour in elision      */
   char     elision;		/* TRUE if this node is collapsed          */
   char     on_path;		/* true if on path to root from node       */
   char     split;              /* flag for drawing subtree contours       */
   char     show_contour;	/* flag to show or hide subtree contour    */
};

typedef enum {
   Erase,
   Draw
} DrawMode;

typedef enum {
   Old,
   New
} PosMode;			/* Position mode */

extern Polyline* MakeLine();
extern Tree*     MakeNode();
extern Tree*     ReadTreeFromFile();
extern void      ComputeTreeSize();
extern void      Unzip();
extern void      Zip();

extern Tree     *TheTree;
extern int       NumLines;
extern int       NumNodes;
