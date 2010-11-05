/* ----------------------------------------------------------------------------
 * File    : tree.c
 * Purpose : dynamic tree program based on Sven Moen's algorithm
 * ----------------------------------------------------------------------------
 */

#include "defs.h"
#include "tree.h"
#include "dbl.h"
#include "intf.h"
#include <string.h>

/* ------------------------------------------------------------------------- */
/*				Global Variables                             */
/* ------------------------------------------------------------------------- */

int NumLines = 0;
int NumNodes = 0;


/* ----------------------------------------------------------------------------
 * 
 *   MakeLine() allocates the memory required for a Polyline and 
 *   initializes the fields of a Polyline to the arguments. The
 *   newly-allocated Polyline is returned by the function.
 * 
 * ----------------------------------------------------------------------------
 */

Polyline*
MakeLine(dx, dy, link)
   short dx;
   short dy;
   Polyline *link;
{
   Polyline *new;

   new = (Polyline *) malloc(sizeof(Polyline));
   NASSERT(new, "could not allocate memory for polyline");
   NumLines++;

   new->dx = dx;
   new->dy = dy;
   new->link = link;

   return (new);
}

/* ----------------------------------------------------------------------------
 * 
 *   MakeNode() allocates the memory required for a tree node, and
 *   zeros out all the fields in the Node. It returns a pointer to the
 *   tree node upon success, and NULL upon failure.
 * 
 * ----------------------------------------------------------------------------
 */

Tree*
MakeNode()
{
   Tree *node;
   
   node = (Tree *) malloc(sizeof(Tree));
   NASSERT(node, "could not allocate memory for node");
   NumNodes++;

   if (node == NULL)
      return (NULL);
   else {
#ifdef SYSV
      memset((char *) node, 0, sizeof(Tree));
#else
      bzero((char *) node, sizeof(Tree));
#endif
      return (node);
   }
}

/* ----------------------------------------------------------------------------
 * 
 *   MakeBridge()
 * 
 * ----------------------------------------------------------------------------
 */

Polyline*
MakeBridge(line1, x1, y1, line2, x2, y2)
   Polyline *line1, *line2;
   int x1, x2, y1, y2;
{
   int dx, dy, s;
   Polyline *r;

   dx = x2 + line2->dx - x1;
   if (line2->dx == 0)
      dy = line2->dy;
   else {
      s = dx * line2->dy;
      dy = s / line2->dx;
   }
   r = MakeLine(dx, dy, line2->link);
   line1->link = MakeLine(0, y2 + line2->dy - dy - y1, r);

   return (r);
}

/* ----------------------------------------------------------------------------
 * 
 *   Offset() computes the necessary offset that prevents two line segments
 *   from intersecting each other. This is the "heart" of the merge step
 *   that computes how two subtree contours should be separated. 
 * 
 *   The code is taken directly from Sven Moen's paper, with changes in
 *   some variable names to give more meaning: 
 *   
 *   - px,py indicate the x- and y-coordinates of the point on the longer
 *     segment if the previous Offset() call had two unequal segments
 * 
 *   - lx,ly indicate the dx and dy values of the "lower" line segment
 * 
 *   - ux,uy indicate the dx and dy values of the "upper" line segment
 * 
 * ----------------------------------------------------------------------------
 */

int
Offset(px, py, lx, ly, ux, uy)
   int px, py, lx, ly, ux, uy;
{
   int d, s, t;

   if (ux <= px || px+lx <= 0)
      return 0;

   t = ux*ly - lx*uy;

   if (t > 0) {
      if (px < 0) {
	 s = px*ly;
	 d = s/lx - py;
      }
      else if (px > 0) {
	 s = px*uy;
	 d = s/ux - py;
      }
      else {
	 d = -py;
      }
   }
   else {
      if (ux < px+lx) {
	 s = (ux-px) * ly;
	 d = uy - (py + s/lx);
      }
      else if (ux > px+lx) {
	 s = (lx+px) * uy;
	 d = s/ux - (py+ly);
      }
      else {
	 d = uy - (py+ly);
      }
   }

   return MAX(0, d);
}

/* ----------------------------------------------------------------------------
 * 
 *   Merge()
 * 
 * ----------------------------------------------------------------------------
 */

int
Merge(c1, c2)
   Polygon *c1, *c2;
{
   int x, y, total, d;
   Polyline *lower, *upper, *bridge;

   x = y = total = 0;

   /*  compare lower part of upper child's contour 
    *  with upper part of lower child's contour
    */
   upper = c1->lower.head;
   lower = c2->upper.head;

   while (lower && upper) {
      d = Offset(x, y, lower->dx, lower->dy, upper->dx, upper->dy);
      y += d;
      total += d;

      if (x + lower->dx <= upper->dx) {
	 x += lower->dx;
	 y += lower->dy;
	 lower = lower->link;
      }
      else {
	 x -= upper->dx;
	 y -= upper->dy;
	 upper = upper->link;
      }
   }
	 
   if (lower) {
      bridge = MakeBridge(c1->upper.tail, 0, 0, lower, x, y);
      c1->upper.tail = (bridge->link) ? c2->upper.tail : bridge;
      c1->lower.tail = c2->lower.tail;
   }
   else {
      bridge = MakeBridge(c2->lower.tail, x, y, upper, 0, 0);
      if (!bridge->link) 
	 c1->lower.tail = bridge;
   }
   c1->lower.head = c2->lower.head;

   return (total);
}

/* ----------------------------------------------------------------------------
 * 
 *   DetachParent() reverses the effects of AttachParent by removing
 *   the four line segments that connect the subtree contour to the
 *   node specified by 'tree'. 
 * 
 * ----------------------------------------------------------------------------
 */

void
DetachParent(tree)
   Tree *tree;
{
   free(tree->contour.upper.head->link);
   free(tree->contour.upper.head);
   tree->contour.upper.head = NULL;
   tree->contour.upper.tail = NULL;

   free(tree->contour.lower.head->link);
   free(tree->contour.lower.head);
   tree->contour.lower.head = NULL;
   tree->contour.lower.tail = NULL;

   NumLines -= 4;
}

/* ----------------------------------------------------------------------------
 * 
 *   AttachParent() 
 *   This function also establishes the position of the first child
 *   The code follows Sven Moen's version, with slight modification to
 *   support varying borders at different levels.
 * 
 * ----------------------------------------------------------------------------
 */

void
AttachParent(tree, h)
   Tree *tree;
   int h;
{
   int x, y1, y2;

   if (TreeAlignNodes)
      x = tree->border + (TreeParentDistance * 2) +
	 (TreeParentDistance - tree->width);
   else
      x = tree->border + TreeParentDistance;
   y2 = (h - tree->height)/2 - tree->border;
   y1 = y2 + tree->height + (2 * tree->border) - h; 
   tree->child->offset.x = x + tree->width;
   tree->child->offset.y = y1;
   tree->contour.upper.head = MakeLine(tree->width, 0,
				       MakeLine(x, y1,
						tree->contour.upper.head));
   tree->contour.lower.head = MakeLine(tree->width, 0,
				       MakeLine(x, y2,
						tree->contour.lower.head));
}

/* ----------------------------------------------------------------------------
 * 
 *   Split()
 *   The tree passed to Split() must have at least 1 child, because
 *   it doesn't make sense to split a leaf (there are no bridges)
 * 
 * ----------------------------------------------------------------------------
 */

void
Split(tree)
   Tree *tree;
{
   Tree *child;
   Polyline *link;

   FOREACH_CHILD(child, tree) {
      if (link = child->contour.upper.tail->link) {
	 free(link->link);
	 free(link);
	 child->contour.upper.tail->link = NULL;
	 NumLines -= 2;
      }
      if (link = child->contour.lower.tail->link) {
	 free(link->link);
	 free(link);
	 NumLines -= 2;
	 child->contour.lower.tail->link = NULL;
      }
   }
}

/* ----------------------------------------------------------------------------
 * 
 *   Join() merges all subtree contours of the given tree and returns the
 *   height of the entire tree contour. 
 * 
 * ----------------------------------------------------------------------------
 */

int
Join(tree)
   Tree *tree;
{
   Tree *child;
   int d, h, sum;

   /*   to start, set the parent's contour and height
    *   to contour and height of first child 
    */
   child = tree->child;
   tree->contour = child->contour;
   sum = h = child->height + (2 * child->border);

   /* extend contour to include contours of all children of parent */
   for (child = child->sibling ; child ; child = child->sibling) {
      d = Merge(&tree->contour, &child->contour);
      child->offset.y = d + h;
      child->offset.x = 0;
      h = child->height + (2 * child->border);
      /* keep cumulative heights of subtree contours */
      sum += d + h;
   }
   return sum;
}

/* ----------------------------------------------------------------------------
 * 
 *   RuboutLeaf() accepts a single node (leaf) and removes its contour.
 *   The memory associated with the contour is deallocated. 
 * 
 * ----------------------------------------------------------------------------
 */

void
RuboutLeaf(tree)
   Tree *tree;
{
   free(tree->contour.upper.head);
   free(tree->contour.lower.tail);
   free(tree->contour.lower.head);
   tree->contour.upper.head = NULL;   
   tree->contour.upper.tail = NULL;   
   tree->contour.lower.head = NULL;   
   tree->contour.lower.tail = NULL;   
   NumLines -= 3;
}

/* ----------------------------------------------------------------------------
 * 
 *   LayoutLeaf() accepts a single node (leaf) and forms its contour. This
 *   function assumes that the width, height, and border fields of the 
 *   node have been assigned meaningful values.
 * 
 * ----------------------------------------------------------------------------
 */

void
LayoutLeaf(tree)
   Tree *tree;
{
   tree->node_height = 0;
   tree->border = TreeBorderSize;

   tree->contour.upper.tail = MakeLine(tree->width + 2 * tree->border, 0,
				       NULL);
   tree->contour.upper.head = tree->contour.upper.tail;
   
   tree->contour.lower.tail = MakeLine(0, -tree->height - 2 * tree->border,
				       NULL);
   tree->contour.lower.head = MakeLine(tree->width + 2 * tree->border, 0,
				       tree->contour.lower.tail);

}

/* ----------------------------------------------------------------------------
 * 
 *   LayoutTree() traverses the given tree (in depth-first order), and forms
 *   subtree or leaf contours at each node as needed. Each node's contour is
 *   stored in its "contour" field. Elision is also supported by generating
 *   the contour for both the expanded and collapsed node. This routine
 *   also computes the tree height of each node in the tree, so that variable
 *   density layout can be demonstrated.
 * 
 * ----------------------------------------------------------------------------
 */

void
LayoutTree(tree)
   Tree *tree;
{
   Tree *child;
   int   height = 0;

   FOREACH_CHILD(child, tree) {
      LayoutTree(child);

      if (child->elision) {	/* support elision */
	 child->old_contour = child->contour;
	 LayoutLeaf(child);
      }

   }

   if (tree->child) {

      FOREACH_CHILD(child, tree) 
	 height = MAX(child->node_height, height);
      tree->node_height = height + 1;

      if (TreeLayoutDensity == Fixed)
	 tree->border = TreeBorderSize;
      else
	 tree->border =
	    (int) (TreeBorderSize * (tree->node_height * DENSITY_FACTOR));

      AttachParent(tree, Join(tree));
   }
   else
      LayoutLeaf(tree);
}

/* ------------------------------------------------------------------------- */

void
Unzip(tree)
   Tree *tree;
{
   Tree *child;

#ifdef INTF
   if (TreeShowSteps) {
      HiliteNode(tree, New);
      tree->on_path = TRUE;
      StatusMsg("Unzip: follow parent links up to root");
      Pause();
   }
#endif   

   if (tree->parent)
      Unzip(tree->parent);

   if (tree->child) {

#ifdef INTF
      /*   draw entire contour; do it only for root, because the last
       *   frame drawn in this function will have already drawn the  
       *   contour for the most recently split subtree.              
       */
      if (TreeShowSteps) {
	 if (tree->parent == NULL) {
	    BeginFrame();
	      DrawTreeContour(tree, New, CONTOUR_COLOR, FALSE, FALSE, FALSE);
	      DrawTree(TheTree, New);
	    EndFrame();
	    StatusMsg("Unzip: disassemble entire contour");
	    Pause();
	 }
      }
#endif

#ifdef INTF
      /* draw contour as it would appear after DetachParent() */
      if (TreeShowSteps) {
	 BeginFrame();
	   DrawTreeContour(tree, New, CONTOUR_COLOR, TRUE,
			   FALSE, FALSE, FALSE);
	   DrawTree(TheTree, New);
	 EndFrame();
	 StatusMsg("Unzip: detach parent");
	 Pause();
      }
#endif

      DetachParent(tree);
      Split(tree);

#ifdef INTF
      if (TreeShowSteps) {
	 BeginFrame();
           /* mark other subtree contours as split, and */
	   /* draw only the contour on path in full     */
	   FOREACH_CHILD(child, tree) {
	      if (!child->on_path) 
		 child->split = TRUE;
	      else
		 DrawTreeContour(child, New, CONTOUR_COLOR,
				 FALSE, FALSE, FALSE);
	   }
	   DrawTree(TheTree, New);
	 EndFrame();
	 StatusMsg("Unzip: split tree");
	 Pause();
      }
#endif

   }
   else
      RuboutLeaf(tree);		/* leaf node */
}

/* ------------------------------------------------------------------------- */

void
Zip(tree)
   Tree *tree;
{
   if (tree->child)
      AttachParent(tree, Join(tree));
   else
      LayoutLeaf(tree);

   if (tree->parent)
      Zip(tree->parent);
}

/* ----------------------------------------------------------------------------
 * 
 *   Insert() adds the specified child to parent, just after the specified
 *   sibling. If 'sibling' is Null, the child is added as the first child.
 * 
 * ----------------------------------------------------------------------------
 */

void
Insert(parent, child, sibling)
   Tree *parent, *child, *sibling;
{
   Unzip(parent);
   child->parent = parent;
   if (sibling) {
      child->sibling = sibling->sibling;
      sibling->sibling = child;
   }
   else {
      child->sibling = parent->child;
      parent->child = child;
   }
   Zip(parent);
}




/* ----------------------------------------------------------------------------
 * 
 *   Delete() traverses the specified tree and frees all storage
 *   allocated to the subtree, including contours and bridges.
 *   If the tree had a preceding sibling, the preceding sibling is
 *   modified to point to the tree's succeeding sibling, if any.
 * 
 * ----------------------------------------------------------------------------
 */

Delete(tree)
   Tree *tree;
{
   Tree *sibling = NULL;
   Tree *parent, *child;

   /* find sibling */
   parent = tree->parent;
   if (parent) {
      FOREACH_CHILD(child, parent)
	 if (child->sibling == tree) {
	    sibling = child;
	    break;
	 }
   }
   if (sibling)
      sibling->sibling = tree->sibling;
   else if (parent)
      parent->child = tree->sibling;
   
   DeleteTree(tree, FALSE);
}


/* ----------------------------------------------------------------------------
 * 
 *   DeleteTree() is the recursive function that supports Delete(). 
 *   If 'contour' is True, then only the contours are recursively deleted.
 *   This flag should be True when you are regenerating a tree's layout
 *   and still want to preserve the nodes. Since contours would be deleted
 *   only due to a change in sibling or level distance, each node's border
 *   value is updated with the current value of TreeBorderSize;
 * 
 * ----------------------------------------------------------------------------
 */

DeleteTree(tree, contour)
   Tree *tree;
   int   contour;
{
   Tree *child;

   if (tree->elision) {
      RuboutLeaf(tree);
      tree->contour = tree->old_contour;
      tree->old_contour.upper.head = NULL;    /* flag to note 'NULL' contour */
   }

   if (!IS_LEAF(tree)) {
      DetachParent(tree);
      Split(tree);

      FOREACH_CHILD(child,tree)
	 DeleteTree(child, contour);
   }
   else
      RuboutLeaf(tree);

   if (contour) 
      tree->border = TreeBorderSize;
   else {
      free(tree->label.text);
      free(tree);
      NumNodes--;
   }
}


/* ----------------------------------------------------------------------------
 * 
 *   ComputeTreeSize() 
 *   This function should be called after tree layout.
 * 
 * ----------------------------------------------------------------------------
 */

void
ComputeTreeSize(tree, width, height, x_offset, y_offset)
   Tree *tree;
   int *width, *height;
   int *x_offset, *y_offset;
{
   Polyline *contour, *tail;
   int upper_min_y = 0, lower_max_y = 0;
   int upper_abs_y = 0, lower_abs_y = 0;
   int x = 0;

   /* do upper contour */
   contour = tree->contour.upper.head;
   tail    = tree->contour.upper.tail;
   while (contour) {
      if ((contour->dy + upper_abs_y) < upper_min_y) 
	 upper_min_y = contour->dy + upper_abs_y;
      upper_abs_y += contour->dy;
      if (contour == tail)
	 contour = NULL;
      else
	 contour = contour->link;
   }

   /* do lower contour */
   contour = tree->contour.lower.head;
   tail    = tree->contour.lower.tail;
   while (contour) {
      if ((contour->dy + lower_abs_y) > lower_max_y)
	 lower_max_y = contour->dy + lower_abs_y;
      lower_abs_y += contour->dy;
      x += contour->dx;
      if (contour == tail)
	 contour = NULL;
      else
	 contour = contour->link;
   }

   *width = x + 1;
   *height = lower_max_y - upper_min_y +
             (tree->height + (2 * tree->border)) + 1;
   if (x_offset)
      *x_offset = tree->border;
   if (y_offset)
      *y_offset = - upper_min_y + tree->border;
}

/* ----------------------------------------------------------------------------
 * 
 *   PetrifyTree()
 * 
 * ----------------------------------------------------------------------------
 */

void
PetrifyTree(tree, x, y)
   Tree *tree;
   int x, y;
{
   int width, height;
   int x_offset, y_offset;
   
   tree->old_pos = tree->pos;	/* used by AnimateTree */

   /* fix position of each node */
   tree->pos.x = x + tree->offset.x;
   tree->pos.y = y + tree->offset.y;
   
   if (tree->child) {
      PetrifyTree(tree->child, tree->pos.x, tree->pos.y);
      ComputeSubTreeExtent(tree); /* for benefit of interface picking */
   }
   if (tree->sibling)
      PetrifyTree(tree->sibling, tree->pos.x, tree->pos.y);
}
