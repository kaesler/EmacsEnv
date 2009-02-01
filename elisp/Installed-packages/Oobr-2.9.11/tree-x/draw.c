/* ----------------------------------------------------------------------------
 * File    : draw.c
 * Purpose : drawing-specific routines for dynamic tree program
 * ----------------------------------------------------------------------------
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "defs.h"
#include "tree.h"
#include "dbl.h"
#include "intf.h"

/* ------------------------------------------------------------------------- */
/*				Global Variables                             */
/* ------------------------------------------------------------------------- */

Tree *TheTree;


/* ------------------------------------------------------------------------- */
/*				Local Variables                              */
/* ------------------------------------------------------------------------- */

static char AnimationMode = FALSE;        
static char strbuf[BUFSIZ];
static int  AnimationStep = ANIMATION_STEP;

/* ------------------------------------------------------------------------- */
/*			 Forward Function Declarations                       */
/* ------------------------------------------------------------------------- */

void DrawNode();
void DrawTreeContour();


/* ------------------------------------------------------------------------- */
/*				   Functions                                 */
/* ------------------------------------------------------------------------- */


/* ----------------------------------------------------------------------------
 * 
 *   BeginFrame() provides an abstraction for double buffering. It should
 *   be called prior to creating a new frame of animation.
 * 
 * ----------------------------------------------------------------------------
 */

void
BeginFrame()
{
  DBLbegin_frame(TreeDrawingAreaDB);
}


/* ----------------------------------------------------------------------------
 * 
 *   EndFrame() provides an abstraction for double buffering. It should
 *   be called after creating a new frame of animation.
 * 
 * ----------------------------------------------------------------------------
 */

void
EndFrame()
{
  DBLend_frame(TreeDrawingAreaDB, 0);
}


/* ----------------------------------------------------------------------------
 * 
 *   GetDrawingSize() gets the size of the drawing area, and returns the
 *   dimensions in the arguments.
 * 
 * ----------------------------------------------------------------------------
 */

void
GetDrawingSize(width, height)
     int *width, *height;
{
  Dimension w, h;
  
  XtVaGetValues(TreeDrawingArea, 
		XtNwidth, &w,
		XtNheight, &h,
		NULL);

  *width  = (int) w;
  *height = (int) h;
}


/* ----------------------------------------------------------------------------
 * 
 *   SetDrawingSize() sets the size of the drawing area to the given
 *   dimensions. 
 * 
 * ----------------------------------------------------------------------------
 */

void
SetDrawingSize(width, height)
     int width, height;
{
  XtVaSetValues(TreeDrawingArea,
		XtNwidth, (Dimension) width,
		XtNheight,  (Dimension) height,
		NULL);
}


/* ----------------------------------------------------------------------------
 * 
 *   SetDrawingTree() is used to specify what tree is to be drawn in the
 *   drawing area. 
 * 
 * ----------------------------------------------------------------------------
 */

void
SetDrawingTree(tree)
   Tree *tree;
{
   TheTree = tree;
}


/* ----------------------------------------------------------------------------
 * 
 *   SetNodeLabel() sets the label text of the specified node and computes
 *   the bounding rectangle so that the layout can be determined. This
 *   function is called when new nodes are created. If TreeAlignNodes is
 *   True, the string is truncated so that the node's width is no longer
 *   than TreeParentDistance.
 * 
 * ----------------------------------------------------------------------------
 */

void
SetNodeLabel(node, label)
     Tree *node;
     char *label;
{
  int         len;
  int         dummy;
  XCharStruct rtrn;
  
  len = strlen(label);
  while (len > 1) {
    XTextExtents(TreeLabelFont, label, len, &dummy, &dummy, &dummy, &rtrn);
    node->width  = rtrn.lbearing + rtrn.rbearing + (LABEL_MAT_WIDTH * 2) + 1;
    node->height = rtrn.ascent + rtrn.descent + (LABEL_MAT_HEIGHT * 2) + 1;
    if (TreeAlignNodes) {
      if (node->width <= (2 * TreeParentDistance))
	break;
      else
	len--;
    }
    else
      break;
  }
  
  node->label.text = label;
  node->label.len  = len;
  node->label.xoffset = LABEL_MAT_WIDTH + 1,
  node->label.yoffset = rtrn.ascent + LABEL_MAT_HEIGHT + 1;
}


/* ----------------------------------------------------------------------------
 * 
 *   SetDrawColor() sets the drawing color of the TreeDrawingArea. 
 * 
 * ----------------------------------------------------------------------------
 */

void
SetDrawColor(color)
     int color;
{
  XSetForeground(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc,
		 TreeDrawingAreaDB->colors[color]);
}

/* ----------------------------------------------------------------------------
 * 
 *   SetLineWidth() sets the line width of lines drawn in the TreeDrawingArea.
 * 
 * ----------------------------------------------------------------------------
 */

void
SetLineWidth(width)
     unsigned int width;
{
  XSetLineAttributes(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc,
		     width, LineSolid, CapButt, JoinRound);
}


/* ----------------------------------------------------------------------------
 * 
 *   SetContours() sets the visibility of three possible contour modes: 
 *   the outside contour, all subtree contours, or selected contours.
 * 
 * ----------------------------------------------------------------------------
 */

void
SetContours(option)
     ContourOption option;
{
  if (option == NoContours) {
    switch (TreeShowContourOption) {
    case OutsideContour:
      DrawTreeContour(TheTree, New, BACKGROUND_COLOR, FALSE, FALSE, FALSE);
      break;
    case AllContours:
      DrawTreeContour(TheTree, New, BACKGROUND_COLOR, FALSE, FALSE, TRUE);
      break;
    case SelectedContours:
      DrawTreeContour(TheTree, New, BACKGROUND_COLOR, FALSE, TRUE, TRUE);
      break;
    default:
      ;
    }
    DrawTreeContour(TheTree, New, BACKGROUND_COLOR, FALSE, FALSE, TRUE);
  }
  else if (option == OutsideContour) {
    switch (TreeShowContourOption) {
    case AllContours:
      DrawTreeContour(TheTree, New, BACKGROUND_COLOR, FALSE, FALSE, TRUE);
      break;
    case SelectedContours:
      DrawTreeContour(TheTree, New, BACKGROUND_COLOR, FALSE, TRUE, TRUE);
      break;
    default:
      ;
    }
    DrawTreeContour(TheTree, New, CONTOUR_COLOR, FALSE, FALSE, FALSE);
  } else if (option == AllContours) {
    DrawTreeContour(TheTree, New, CONTOUR_COLOR, FALSE, FALSE, TRUE);
  } else if (option == SelectedContours) {
    switch (TreeShowContourOption) {
    case AllContours:
      DrawTreeContour(TheTree, New, BACKGROUND_COLOR, FALSE, FALSE, TRUE);
      break;
    case OutsideContour:
      DrawTreeContour(TheTree, New, BACKGROUND_COLOR, FALSE, FALSE, FALSE);
      break;
    default:
      DrawTreeContour(TheTree, New, BACKGROUND_COLOR, FALSE, FALSE, TRUE);
    }
    DrawTreeContour(TheTree, New, CONTOUR_COLOR, FALSE, TRUE, TRUE);
  }
  TreeShowContourOption = option;
}


/* ----------------------------------------------------------------------------
 * 
 *   HiliteNode() is called by Unzip() to change the color of a node. 
 * 
 * ----------------------------------------------------------------------------
 */

void
HiliteNode(tree, pos_mode)
     Tree *tree;
{
  SetDrawColor(HIGHLIGHT_COLOR);
  DrawNode(tree, pos_mode);
  SetDrawColor(TREE_COLOR);
}


/* ----------------------------------------------------------------------------
 * 
 *   DrawNode() takes a node and draws the node in the specified widget
 *   at its (x,y) coordinate. (x, y) indicates the upper-left corner where
 *   the node is drawn. Also, a line is drawn from the center of the left
 *   edge to the center of the parent's right edge. 'draw_mode' specifies 
 *   the drawing mode (whether or not the node is erased, rather than drawn).
 *   'pos_mode' determines whether or not to use the old position of the node.
 *   This flag is used in animating the movement of a node from its old
 *   position to its new position.
 * 
 * ----------------------------------------------------------------------------
 */

void
DrawNode(node, pos_mode)
     Tree     *node;
     PosMode  pos_mode;
{
  Widget   w;
  GC       gc;
  
  w  = TreeDrawingArea;
  gc = TreeDrawingAreaDB->gc;
  
  if (pos_mode == Old) {
    XDrawString(XtDisplay(w), XtWindow(w), gc,
		node->old_pos.x + node->label.xoffset,
		node->old_pos.y + node->label.yoffset,
		node->label.text, node->label.len);
    XDrawRectangle(XtDisplay(w), XtWindow(w), gc,
		   node->old_pos.x, node->old_pos.y,
		   node->width, node->height);
    if (node->parent) 
      XDrawLine(XtDisplay(w), XtWindow(w), gc,
		node->old_pos.x - 1,
		node->old_pos.y + (node->height / 2),
		node->parent->old_pos.x + node->parent->width + 1,
		node->parent->old_pos.y + (node->parent->height / 2));
    if (node->elision) {
      XSetFillStyle(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc,
		    FillTiled);
      XFillRectangle(XtDisplay(w), XtWindow(w), gc,
		     node->old_pos.x + node->width - ELISION_WIDTH,
		     node->old_pos.y + 1, ELISION_WIDTH, node->height - 1);
      XSetFillStyle(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc,
		    FillSolid);
    }
  } else {
    XDrawString(XtDisplay(w), XtWindow(w), gc,
		node->pos.x + node->label.xoffset,
		node->pos.y + node->label.yoffset,
		node->label.text, node->label.len);
    
    XDrawRectangle(XtDisplay(w), XtWindow(w), gc,
		   node->pos.x, node->pos.y,
		   node->width, node->height);
    if (node->parent) 
      XDrawLine(XtDisplay(w), XtWindow(w), gc,
		node->pos.x - 1,
		node->pos.y + (node->height / 2),
		node->parent->pos.x + node->parent->width + 1,
		node->parent->pos.y + (node->parent->height / 2));
    if (node->elision) {
      XSetFillStyle(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc,
		    FillTiled);
      XFillRectangle(XtDisplay(w), XtWindow(w), gc,
		     node->pos.x + node->width - ELISION_WIDTH,
		     node->pos.y + 1, ELISION_WIDTH, node->height - 1);
      XSetFillStyle(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc,
		    FillSolid);
    }
  }
}


/* ----------------------------------------------------------------------------
 * 
 *   DrawTreeContour() draws the contour of the specified subtree. Bridges
 *   are not traversed, so the actual subtree contour is drawn, as opposed
 *   to the merged contour. 'color' specifies the drawing color. If 'detach'
 *   is True,  the lines attaching the subtree contour to the node are not
 *   drawn.  If 'select' is true, then only subtrees that are flagged as
 *   selected are shown. If 'recursive' is True, the entire tree is traversed.
 * 
 * ----------------------------------------------------------------------------
 */

void
DrawTreeContour(tree, pos_mode, color, detach, select, recursive)
     Tree *tree;
     PosMode pos_mode;
     int color;
     int detach;
     int select;
     int recursive;
{
  Widget w = TreeDrawingArea;
  Polyline *contour, *tail;
  Tree *child;
  int x, y, i;

  if (tree == NULL)
    return;

  if ((select && tree->show_contour) || !select) {

    SetDrawColor(color);
    SetLineWidth(TreeContourWidth);
   
    /* draw upper contour */
    contour = tree->contour.upper.head;
    tail    = tree->contour.upper.tail;
    if (pos_mode == Old) {
      x = tree->old_pos.x - tree->border;
      y = tree->old_pos.y - tree->border;
    }
    else {
      x = tree->pos.x - tree->border;
      y = tree->pos.y - tree->border;
    }

    if (detach) {		/* skip over attaching lines */
      for (i = 0 ; i < 2 ; i++) {
	x += contour->dx;
	y += contour->dy;
	contour = contour->link;
      }
    }
    
    while (contour) {
      XDrawLine(XtDisplay(w), XtWindow(w), TreeDrawingAreaDB->gc,
		x, y, x + contour->dx, y + contour->dy);
      x += contour->dx;
      y += contour->dy;
      if (contour == tail)	/* make sure you don't follow bridges */
	contour = NULL;
      else
	contour = contour->link;
    }

    /* draw lower contour */
    contour = tree->contour.lower.head;
    tail    = tree->contour.lower.tail;
    if (pos_mode == Old) {
      x = tree->old_pos.x - tree->border;
      y = tree->old_pos.y + tree->border + tree->height;
    } else {
      x = tree->pos.x - tree->border;
      y = tree->pos.y + tree->border + tree->height;
    }

    if (detach) {		/* skip over attaching lines */
      for (i = 0 ; i < 2 ; i++) {
	x += contour->dx;
	y += contour->dy;
	contour = contour->link;
      }
    }

    while (contour) {
      XDrawLine(XtDisplay(w), XtWindow(w), TreeDrawingAreaDB->gc,
		x, y, x + contour->dx, y + contour->dy);
      x += contour->dx;
      y += contour->dy;
      if (contour == tail)	/* make sure you don't follow bridges */
	contour = NULL;
      else
	contour = contour->link;
    }
  }
  
  if (recursive) {
    FOREACH_CHILD(child, tree)
      if (!child->elision)
	DrawTreeContour(child, pos_mode, color,
			detach, select, recursive);
  }

  SetDrawColor(TREE_COLOR);
  SetLineWidth(0);
}


/* ----------------------------------------------------------------------------
 * 
 *   DrawTree() traverses the given tree, drawing the node and connecting
 *   segments. The tree contours are also drawn at each step, if enabled.
 *   'draw_mode' specifies the drawing mode in which the tree is drawn.
 *   'pos_mode' determines whether or not to use the old position of the node.
 *   This flag is used in animating the movement of a node from its old
 *   position to its new position. DrawNode() is called to draw an individual 
 *   node.
 * 
 * ----------------------------------------------------------------------------
 */

void
DrawTree(tree, pos_mode)
     Tree     *tree;
     PosMode  pos_mode;
{
  if (tree == NULL)
    return;

  DrawNode(tree, pos_mode);

  /* do stuff that animates Unzip() */
  if (tree->split) {
    if (!AnimationMode ||
	(tree->pos.x == tree->old_pos.x &&
	 tree->pos.y == tree->old_pos.y))
      DrawTreeContour(tree, pos_mode, SPLIT_COLOR, FALSE, FALSE, FALSE);
    else
      DrawTreeContour(tree, pos_mode, ACTION_COLOR, FALSE, FALSE, FALSE);
  }
  if (tree->on_path)
    HiliteNode(tree, pos_mode);

  if (tree->child && !tree->elision) 
    DrawTree(tree->child, pos_mode);
  if (tree->sibling)
    DrawTree(tree->sibling, pos_mode);
}


/* ----------------------------------------------------------------------------
 * 
 *   ShiftTree() adjusts the positions of each node so that it moves from
 *   the "old" position towards the "new position". This is used by
 *   AnimateTree(). 'done' is set to FALSE if the tree is not in its
 *   final position; it is used to determine when to stop animating the tree.
 * 
 * ----------------------------------------------------------------------------
 */

void
ShiftTree(tree, done)
     Tree *tree;
     int  *done;
{
  Tree *child;
  
  if (tree->old_pos.x != tree->pos.x ||
      tree->old_pos.y != tree->pos.y)
    {
      tree->old_pos.x = tree->pos.x;
      tree->old_pos.y = tree->pos.y;
    }
  
  FOREACH_CHILD(child, tree)
    ShiftTree(child, done);
}


/* ----------------------------------------------------------------------------
 * 
 *   AnimateTree() draws the given tree in a series of steps to give the
 *   effect of animation from the "old" layout to the "new" layout of the
 *   tree. 
 * 
 *   The algorithm used here is not efficient; the entire tree is drawn
 *   on each iteration of the animation sequence; it would be more efficient
 *   to only redraw what is necessary. However, the method used here takes
 *   advantage of existing code without modification.
 * 
 * ----------------------------------------------------------------------------
 */

void
AnimateTree(tree)
     Tree *tree;
{
  int done = FALSE;

  AnimationMode = FALSE;
  /* highlight which nodes have to move */
  BeginFrame();
    DrawTree(tree, Old);
  EndFrame();
  Pause(); 
  if (PauseAfterStep)
    AnimationStep = ANIMATION_STEP_STEP;
  while (!done) {
    done = TRUE;
    ShiftTree(tree, &done);
    BeginFrame();
      DrawTree(tree, Old);
    EndFrame();
    if (PauseAfterStep)
      Pause();
  }
  if (PauseAfterStep)
    AnimationStep = ANIMATION_STEP;
  AnimationMode = FALSE;
}


/* ----------------------------------------------------------------------------
 * 
 *   AnimateZip() generates a sequence of frames that animates the Zip() step.
 *   It is similar in logical structure to Zip().
 * 
 * ----------------------------------------------------------------------------
 */

void 
AnimateZip(tree)
     Tree *tree;
{
  Tree *child;
   
  /* show results of Join() step */
  if (tree->child) {
    BeginFrame();
      FOREACH_CHILD(child, tree)
        child->split = FALSE;
      DrawTree(TheTree, New);
      DrawTreeContour(tree, New, CONTOUR_COLOR, TRUE, FALSE, FALSE);
    EndFrame();

    StatusMsg("Zip: merge and join contours", FALSE);
    Pause(); 
   
    /* show results of AttachParent() step */
    BeginFrame();
      DrawTree(TheTree, New);
      DrawTreeContour(tree, New, CONTOUR_COLOR, FALSE, FALSE, FALSE);
    EndFrame();

    StatusMsg("Zip: attach parents", FALSE);
    Pause(); 
  }
  
  tree->on_path = FALSE;
   
  if (tree->parent)
    AnimateZip(tree->parent);
  else {
    tree->on_path = FALSE;
    BeginFrame();
      DrawTree(TheTree, New);
      DrawTreeContour(TheTree, New, CONTOUR_COLOR, FALSE, FALSE, FALSE);
    EndFrame();
    StatusMsg("Zip: reassemble entire contour", FALSE);
    Pause(); 
  }
}


/* ----------------------------------------------------------------------------
 * 
 *   CountNodes() returns the number of nodes in the specified tree. 
 *   Nodes below a node that has been collapsed are ignored. 
 * 
 * ----------------------------------------------------------------------------
 */

int
CountNodes(tree)
     Tree *tree;
{
  int num_nodes = 1;		/* count root of subtree */
  Tree *child;
  
  if (!tree->elision) {
    FOREACH_CHILD(child, tree)
      num_nodes += CountNodes(child);
  }
  return (num_nodes);
}


/* ----------------------------------------------------------------------------
 * 
 *   CollectNodeRectangles() is a recursive function used by
 *   GetSubTreeRectangles() to collect the rectangles of descendant nodes
 *   into the pre-allocated storage passed to this function.
 * 
 * ----------------------------------------------------------------------------
 */

void
CollectNodeRectangles(node, rectangles, fill)
     Tree *node;
     XRectangle **rectangles;
     int fill;
{
  Tree *child;
   
  (*rectangles)->x = node->pos.x;
  (*rectangles)->y = node->pos.y;
  if (fill) {
    (*rectangles)->width = node->width + 1;
    (*rectangles)->height = node->height + 1;
  } else {
    (*rectangles)->width = node->width;
    (*rectangles)->height = node->height;
  }
  (*rectangles)++;
  
  if (!node->elision)
    FOREACH_CHILD(child, node) 
      CollectNodeRectangles(child, rectangles, fill);
}


/* ----------------------------------------------------------------------------
 * 
 *   GetSubTreeRectangles() builds an array of XRectangles that contain
 *   all the node rectangles in the tree, except the root node itself. 
 *   The array is returned in 'rectangles' and the number of rectangles
 *   is returned in 'nrectangles.' Storage for the rectangles is allocated
 *   in this function. This function is used by PickAction to determine
 *   what rectangles need to be dissolved away. 'fill', if True, specifies
 *   that the rectangles should be 1 pixel larger in each dimension to 
 *   compensate for FillRectangle behavior. 
 * 
 * ----------------------------------------------------------------------------
 */

void
GetSubTreeRectangles(tree, rectangles, nrectangles, fill)
     Tree *tree;
     XRectangle **rectangles;
     int *nrectangles, fill;
{
  Tree *child;
  XRectangle *crect;		/* current rectangle */
  
  *nrectangles = CountNodes(tree) - 1;        /* don't count root node */
  *rectangles = (XRectangle *) malloc(sizeof(XRectangle) * *nrectangles);
  ASSERT(*rectangles, "could not allocate memory for rectangles");
  
  crect = *rectangles;
  if (!tree->elision)
    FOREACH_CHILD(child, tree)
      CollectNodeRectangles(child, &crect, fill);
}


/* ----------------------------------------------------------------------------
 * 
 *   CollectNodeSegments() is a recursive function used by GetSubTreeSegments()
 *   to collect the line segments connecting nodes into the pre-allocated 
 *   storage passed to this function.
 * 
 * ----------------------------------------------------------------------------
 */

void
CollectNodeSegments(node, segments)
     Tree *node;
     XSegment **segments;
{
  Tree *child;
   
  (*segments)->x1 = node->pos.x - 1;
  (*segments)->y1 = node->pos.y + (node->height / 2),
  (*segments)->x2 = node->parent->pos.x + node->parent->width + 1;
  (*segments)->y2 = node->parent->pos.y + (node->parent->height / 2);
  (*segments)++;

  if (!node->elision)
    FOREACH_CHILD(child, node) 
      CollectNodeSegments(child, segments);
}


/* ----------------------------------------------------------------------------
 * 
 *   GetSubTreeSegments() builds an array of XSegments that contain
 *   all the line segments connecting the nodes in the tree. The array is
 *   returned in 'segments' and the number of segments is returned in
 *   'nsegments.' Storage for the segments is allocated in this function.
 *   This function is used by PickAction to determine what line segments
 *   rectangles need to be dissolved away.
 * 
 * ----------------------------------------------------------------------------
 */

void
GetSubTreeSegments(tree, segments, nsegments)
     Tree *tree;
     XSegment **segments;
     int *nsegments;
{
  Tree *child;
  XSegment *cseg;		/* current segment */

  *nsegments = CountNodes(tree) - 1;
  *segments = (XSegment *) malloc(sizeof(XSegment) * *nsegments);
  ASSERT(*segments, "could not allocate memory for segments");
  
  cseg = *segments;
  if (!tree->elision)
    FOREACH_CHILD(child, tree)
      CollectNodeSegments(child, &cseg);
}


/* ----------------------------------------------------------------------------
 * 
 *   ComputeSubTreeExtent() computes the extent of a subtree. This is
 *   easily computed based on the tree's contour, as in ComputeTreeSize().
 *   This extent is stored in the node, and used by SearchTree for 
 *   pick-correlation. 
 * 
 *   This function assumes that the given tree has at least one child; do not
 *   pass a leaf node to this function. 
 * 
 * ----------------------------------------------------------------------------
 */

void
ComputeSubTreeExtent(tree)
     Tree *tree;
{
  int width, height;
  int x_offset, y_offset;

  ComputeTreeSize(tree, &width, &height, &x_offset, &y_offset);
  tree->subextent.pos.x  = tree->child->pos.x - tree->child->border;
  tree->subextent.pos.y  = tree->pos.y - y_offset;
  tree->subextent.width  = width - (tree->child->pos.x - tree->pos.x) - 1;
  tree->subextent.height = height - 1;
}


/* ----------------------------------------------------------------------------
 * 
 *   SearchTree() determines if a node's rectangular region encloses the
 *   specified point in (x,y). Rather than using a brute-force search 
 *   through all node rectangles of a given tree, the subtree extents
 *   are used in a recursive fashion to drive the search as long as the
 *   given point is enclosed in an extent. In the worst case, the search
 *   time would be on the order of a brute-force search, but with complex
 *   trees, this method reduces the number of visits. 
 * 
 *   The extent of a subtree is computed by ComputeSubTreeExtent() and is
 *   stored in each node of the tree.
 * 
 * ----------------------------------------------------------------------------
 */

int
SearchTree(tree, x, y, node)
     Tree *tree, **node;
     int x, y;
{
  Tree *child;
  
  if (tree == NULL)
    return (FALSE);

  if (PT_IN_RECT(x, y, tree->pos.x, tree->pos.y,
		 tree->pos.x + tree->width,
		 tree->pos.y + tree->height)) {
    *node = tree;
    return (TRUE);
  }
  if (tree->child && (PT_IN_EXTENT(x, y, tree->subextent))) 
    FOREACH_CHILD(child, tree) {
      if (SearchTree(child, x, y, node)) 
	return (TRUE);
    }
  return (FALSE);
}


/* ----------------------------------------------------------------------------
 * 
 *   ExposeHandler() handles expose events in the TreeDrawingArea. This
 *   function is not intelligent; it just redraws the entire contents.
 * 
 * ----------------------------------------------------------------------------
 */

void
ExposeHandler(w, client_data, event)
     Widget   w;
     caddr_t client_data;
     XExposeEvent  *event;
{
  
  if (event->count == 0) {
    BeginFrame();
      SetContours(TreeShowContourOption);
      DrawTree(TheTree, New);
    EndFrame();
  }
}


/* ----------------------------------------------------------------------------
 * 
 *   ExpandCollapseNode is called to expand or collapse a node in the tree.
 * 
 * ----------------------------------------------------------------------------
 */

void
ExpandCollapseNode(node)
     Tree *node;
{
  int        width, height;
  int        old_width, old_height;
  int        x_offset, y_offset;
  XRectangle *rectangles;
  XSegment   *segments;
  int        nrectangles, nsegments;
  int        expand = FALSE;
  Widget     w = TreeDrawingArea;
  
  StatusMsg("", TRUE);
  
  /* hilite node so that we know where we are */
  /* DrawTree will hilite it as a side effect */
  if (TreeShowSteps)
    node->on_path = TRUE;
  
  /* erase the contour before changing in the tree */
  if ((TreeShowContourOption != NoContours) || TreeShowSteps) {
    BeginFrame();
      DrawTree(TheTree, New);
    EndFrame();
  }
   
  sprintf(strbuf, "Node `%s' selected for %s", node->label.text,
	  node->elision ? "expansion" : "collapse");
  StatusMsg(strbuf, FALSE);
  Pause(); 

  if (node->parent)
    Unzip(node->parent);
  else {
    StatusMsg("Show entire contour", FALSE);
    if (TreeShowSteps) {
      BeginFrame();
        DrawTreeContour(TheTree, New, CONTOUR_COLOR, FALSE, FALSE, FALSE);
        DrawTree(TheTree, New);
      EndFrame();
      Pause(); 
    }
  }

  /* are we collapsing a subtree? */
  if (!node->elision) {
    StatusMsg("Collapse subtree", FALSE);
    GetSubTreeRectangles(node, &rectangles, &nrectangles, TRUE);
    GetSubTreeSegments(node, &segments, &nsegments);
    DissolveTree(XtDisplay(w), XtWindow(w),
		 rectangles, nrectangles,
		 segments, nsegments, TRUE);
    free(rectangles);
    free(segments);
    Pause(); 
    
    StatusMsg("Replace subtree contour with leaf contour", FALSE);
    node->elision = TRUE;
    if (TreeShowSteps)
      node->split = TRUE;	/* turned off in AnimateZip */
    node->old_contour = node->contour;
    node->width += ELISION_WIDTH;
    LayoutLeaf(node);
    BeginFrame();
      SetContours(TreeShowContourOption);
      DrawTree(TheTree, New);
    EndFrame();
    Pause();  
  } else {
    StatusMsg("Replace leaf contour with old subtree contour", FALSE);
    if (TreeShowSteps)
      node->split = TRUE;	/* turned off in AnimateZip */
    RuboutLeaf(node);
    node->contour = node->old_contour;
    expand = TRUE;
  }
  
  if (node->parent)
    Zip(node->parent);
  
  ComputeTreeSize(TheTree, &width, &height, &x_offset, &y_offset);
  PetrifyTree(TheTree, x_offset + MAT_SIZE, y_offset + MAT_SIZE);
  GetDrawingSize(&old_width, &old_height);

  if (expand) {
    SetDrawingSize(width + (2 * MAT_SIZE), height + (2 * MAT_SIZE));
    BeginFrame();
      DrawTree(TheTree, Old);
    EndFrame();
    Pause(); 
    StatusMsg("Move tree to new configuration", FALSE);
    AnimateTree(TheTree);
  } else {
    /* we are shrinking or staying the same */
    StatusMsg("Move tree to new configuration", FALSE);
    AnimateTree(TheTree);
    SetDrawingSize(width + (2 * MAT_SIZE), height + (2 * MAT_SIZE));
  }

  if (expand) {
    StatusMsg("Expand subtree", FALSE);
    node->elision = FALSE;
    
    /* erase elision marker */
    XSetFunction(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc,
		 GXclear);
    XFillRectangle(XtDisplay(w), XtWindow(w), TreeDrawingAreaDB->gc,
		   node->pos.x + node->width - ELISION_WIDTH + 1,
		   node->pos.y, ELISION_WIDTH, node->height + 1);
    XSetFunction(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc,
		 GXcopy);
    node->width -= ELISION_WIDTH;
    
    GetSubTreeRectangles(node, &rectangles, &nrectangles, FALSE);
    GetSubTreeSegments(node, &segments, &nsegments);
    /* dissolve the tree back in */
    DissolveTree(XtDisplay(w), XtWindow(w),
		 rectangles, nrectangles,
		 segments, nsegments, FALSE);
    free(rectangles);
    free(segments);
    
    /* draw text of nodes */
    BeginFrame();
      SetContours(TreeShowContourOption);
      DrawTree(TheTree, New);
    EndFrame();
    Pause(); 
  }
  
  if (TreeShowSteps) {
    node->on_path = FALSE;
    if (node->parent)
      AnimateZip(node->parent);
    else
      node->split = FALSE;
  }
  
  /* BUG: the display isn't properly updated here! */
  /* There should probably be some code here that
     clears the tree below the node currently being 
     collapsed or expanded. Hack added 20.03.95 (torgeir@ii.uib.no). 
     I'll try to fix this later. */

  XClearArea(TreeDisplay, XtWindow(TreeDrawingArea), 0, 0, 0, 0, FALSE);

  BeginFrame();
    SetContours(TreeShowContourOption);
    DrawTree(TheTree, New);
  EndFrame();
  
  StatusMsg("Ready", TRUE);
}

/* ----------------------------------------------------------------------------
 * 
 *   InsertNode() handles the task of inserting a new node in the tree,
 *   at the given position with respect to 'base_node'. When 'node_pos' is
 *   either Before or After, it is assumed that 'base_node' has a parent.
 * 
 * ----------------------------------------------------------------------------
 */

void
InsertNode(base_node, node_pos, new_node_text)
     Tree *base_node;
     NodePosition node_pos;
     char *new_node_text;
{
  Tree *new_node;
  Tree *parent;
  Tree *sibling = NULL;
  Tree *child;

  int  width, height;
  int  x_offset, y_offset;

  StatusMsg("", TRUE);

  new_node = MakeNode();	/* should check for memory failure */
  SetNodeLabel(new_node, new_node_text);
  LayoutLeaf(new_node);

  /* figure out parent & sibling */
  if (node_pos == Child) {
    parent = base_node;
    /* find last child, if one exists */
    FOREACH_CHILD(child, parent)
      sibling = child;
  } else if (node_pos == After) {
    parent = base_node->parent;
    sibling = base_node;
  } else if (node_pos == Before) {
    parent = base_node->parent;
    FOREACH_CHILD(child, parent)
      if (child->sibling == base_node) {
	sibling = child;
	break;
      }
  }

  if (TreeShowSteps)
    parent->on_path = TRUE;
  
  if ((TreeShowContourOption != NoContours) ||
      TreeShowSteps) {
    BeginFrame();
      DrawTree(TheTree, New);
    EndFrame();
  }

  sprintf(strbuf, "Inserting `%s' as child of node `%s'",
	  new_node_text, parent->label.text);
  StatusMsg(strbuf, FALSE);
  Pause(); 
   
  /* erase the contour before changing in the tree */
  
  Insert(parent, new_node, sibling);
  
  ComputeTreeSize(TheTree, &width, &height, &x_offset, &y_offset);
  PetrifyTree(TheTree, x_offset + MAT_SIZE, y_offset + MAT_SIZE);
  
  if (sibling)
    new_node->old_pos = sibling->old_pos;
  else if (new_node->sibling)
    new_node->old_pos = new_node->sibling->old_pos;
  else {
    new_node->old_pos.x = new_node->pos.x;
    new_node->old_pos.y = parent->old_pos.y;
  }
  
  if (TreeShowSteps)
    new_node->split = TRUE;

  SetDrawingSize(width + (2 * MAT_SIZE), height + (2 * MAT_SIZE));
  BeginFrame();
    DrawTree(TheTree, Old);
  EndFrame();
  StatusMsg("Insert: add new node and contour", FALSE);
  Pause(); 
   
  StatusMsg("Move tree to new configuration", FALSE);
  AnimateTree(TheTree);

  if (TreeShowSteps) {
    if (parent)
      AnimateZip(parent);
  }

  BeginFrame();
    SetContours(TreeShowContourOption);
    DrawTree(TheTree, New);
  EndFrame();

  StatusMsg("Ready", TRUE);
}   

/* ----------------------------------------------------------------------------
 * 
 *   DeleteNode() handles the task of deleting a given node in the tree.
 * 
 * ----------------------------------------------------------------------------
 */

void
DeleteNode(node)
     Tree *node;
{
  Tree *parent;

  XRectangle *rectangles;
  XSegment   *segments;
  int         nrectangles, nsegments;
  Widget      w = TreeDrawingArea;
  int  width, height;
  int  x_offset, y_offset;
  
  StatusMsg("", TRUE);

  if (TreeShowSteps)
    node->on_path = TRUE;
   
  /* erase the contour before changing in the tree */
  if ((TreeShowContourOption != NoContours) ||
      TreeShowSteps) {
    BeginFrame();
      DrawTree(TheTree, New);
    EndFrame();
  }

  sprintf(strbuf, "Node `%s' selected for deletion", node->label.text);
  StatusMsg(strbuf, FALSE);
  Pause(); 
  
  parent = node->parent;
  
  if (parent)
    Unzip(parent);
  else
    TheTree = NULL;		/* delete root of tree */

  /* fade out deleted subtree */
  StatusMsg("Delete subtree", FALSE);
  GetSubTreeRectangles(node, &rectangles, &nrectangles, TRUE);
  GetSubTreeSegments(node, &segments, &nsegments);
  DissolveTree(XtDisplay(w), XtWindow(w),
	       rectangles, nrectangles,
	       segments, nsegments, TRUE);
  free(rectangles);
  free(segments);

  Delete(node);

  BeginFrame();
  if (TheTree) 
    DrawTree(TheTree, New);
  EndFrame();
  Pause(); 

  if (parent)
    Zip(parent);
  
  if (TheTree) {
    ComputeTreeSize(TheTree, &width, &height, &x_offset, &y_offset);
    PetrifyTree(TheTree, x_offset + MAT_SIZE, y_offset + MAT_SIZE);
    StatusMsg("Move tree to new configuration", FALSE);
    AnimateTree(TheTree);
    SetDrawingSize(width + (2 * MAT_SIZE), height + (2 * MAT_SIZE));
    Pause(); 

    if (TreeShowSteps) {
      if (parent)
	AnimateZip(parent);
    }

    BeginFrame();
      SetContours(TreeShowContourOption);
      DrawTree(TheTree, New);
    EndFrame();

  }

  StatusMsg("Ready", TRUE);
}


/* ----------------------------------------------------------------------------
 * 
 *   ResetLabels() is called when the TreeAlignNodes mode is changed. 
 *   When TreeParentDistance changes, the node width changes, so this
 *   function forces each node's width to be recomputed. 
 * 
 * ----------------------------------------------------------------------------
 */

ResetLabels(tree)
     Tree *tree;
{
  Tree *child;
  
  SetNodeLabel(tree, tree->label.text);
  FOREACH_CHILD(child, tree)
    ResetLabels(child);
}


/* ----------------------------------------------------------------------------
 * 
 *   SetupTree() handles the task of setting up the specified tree in 
 *   the drawing area.
 * 
 * ----------------------------------------------------------------------------
 */

void
SetupTree(tree)
     Tree *tree;
{
  int width, height;
  int x_offset, y_offset;
  
  LayoutTree(tree);
  ComputeTreeSize(tree, &width, &height, &x_offset, &y_offset);
  PetrifyTree(tree, x_offset + MAT_SIZE, y_offset + MAT_SIZE);
  SetDrawingTree(tree);
  SetDrawingSize(width + (2 * MAT_SIZE), height + (2 * MAT_SIZE));
  BeginFrame();
    SetContours(TreeShowContourOption);
    DrawTree(tree, New);
  EndFrame();
}






