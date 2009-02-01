/* ----------------------------------------------------------------------------
 * Animated display of collapse and expansion of nodes in a tree.
 * ----------------------------------------------------------------------------
 */

#include <X11/Xlib.h>

#include "dissolve.h"

static Pixmap DissolvePixmaps[NUM_DISSOLVE_STEPS];
static GC     DissolveInGC;
static GC     DissolveOutGC;
static GC     DissolveInLineGC[NUM_LINE_STEPS];
static GC     DissolveOutLineGC[NUM_LINE_STEPS];

static unsigned char first_dash[] =  {1, 3}; 
static unsigned char second_dash[] = {1, 1}; 

InitializeDissolveEffect(dpy, drawable, fg_pixel, bg_pixel)
   Display *dpy;
   Drawable drawable;
   int fg_pixel;
   int bg_pixel;
{   
   unsigned long  gcvaluemask;
   XGCValues      gcvalues;
   int i;

   /* make DissolveOutGC */
   gcvalues.background = bg_pixel; 
   gcvalues.foreground = bg_pixel;
   gcvalues.function   = GXcopy;
   gcvalues.fill_style = FillStippled;
   gcvalues.line_width = 0;
   gcvaluemask = GCFunction | GCForeground | GCBackground | GCFillStyle |
                 GCLineWidth;
   DissolveOutGC = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetTSOrigin(dpy, DissolveOutGC, 0, 0);

   /* make DissolveInGC */
   gcvalues.foreground = fg_pixel;
   DissolveInGC = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetTSOrigin(dpy, DissolveInGC, 0, 0);

   /* make DissolveOutLineGC */
   i = 0;
   gcvalues.foreground = bg_pixel;
   gcvalues.fill_style = FillSolid;
   gcvalues.line_style = LineOnOffDash;
   gcvalues.line_width = 0;
   gcvaluemask = GCFunction | GCForeground | GCBackground |
                 GCFillStyle | GCLineStyle | GCLineWidth ;
   DissolveOutLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveOutLineGC[i], 0, first_dash, 2);
   i++;
   DissolveOutLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveOutLineGC[i], 0, second_dash, 2);
   i++;
   DissolveOutLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveOutLineGC[i], 3, first_dash, 2);
   i++;
   gcvalues.line_style = LineSolid;
   DissolveOutLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);   

   /* make DissolveInLineGC */
   i = 0;
   gcvalues.foreground = fg_pixel;
   gcvalues.fill_style = FillSolid;
   gcvalues.line_style = LineOnOffDash;
   gcvalues.line_width = 0;
   gcvaluemask = GCFunction | GCForeground | GCBackground |
                 GCFillStyle | GCLineStyle | GCLineWidth;
   DissolveInLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveInLineGC[i], 0, first_dash, 2);
   i++;
   DissolveInLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveInLineGC[i], 0, second_dash, 2);
   i++;
   DissolveInLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveInLineGC[i], 3, first_dash, 2);
   i++;
   gcvalues.line_style = LineSolid;
   DissolveInLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);   

   i = 0;
   DissolvePixmaps[i] = XCreateBitmapFromData(dpy, drawable,
					      first_bits,
					      first_width,
					      first_height);
   i++;
   DissolvePixmaps[i] = XCreateBitmapFromData(dpy, drawable,
					      second_bits,
					      second_width,
					      second_height);
   i++;
   DissolvePixmaps[i] = XCreateBitmapFromData(dpy, drawable,
					      third_bits,
					      third_width,
					      third_height);
   i++;
   DissolvePixmaps[i] = XCreateBitmapFromData(dpy, drawable,
					      fourth_bits,
					      fourth_width,
					      fourth_height);
   i++;
   DissolvePixmaps[i] = XCreateBitmapFromData(dpy, drawable,
					      fifth_bits,
					      fifth_width,
					      fifth_height);
   i++;
   DissolvePixmaps[i] = XCreateBitmapFromData(dpy, drawable,
					      sixth_bits,
					      sixth_width,
					      sixth_height);
   i++;
   DissolvePixmaps[i] = XCreateBitmapFromData(dpy, drawable,
					      seventh_bits,
					      seventh_width,
					      seventh_height);
   i++;
   DissolvePixmaps[i] = XCreateBitmapFromData(dpy, drawable,
					      eighth_bits,
					      eighth_width,
					      eighth_height);
}

DissolveRectangle(dpy, drawable, x, y, width, height, mode)
   Display *dpy;
   Window drawable;
   int x, y, width, height;
   int mode;
{
   int i;
   GC gc;

   gc = mode ? DissolveOutGC : DissolveInGC;

   for (i = 0 ; i < NUM_DISSOLVE_STEPS ; i++) {
      XSetStipple(dpy, gc, DissolvePixmaps[i]);
      if (mode)
	 XFillRectangle(dpy, drawable, gc, x, y, width, height);
      else
	 XDrawRectangle(dpy, drawable, gc, x, y, width, height);
      XFlush(dpy);
      usleep(50000);
   }
}
   
DissolveRectangles(dpy, drawable, rectangles, nrectangles, mode)
   Display *dpy;
   Window drawable;
   XRectangle rectangles[];
   int nrectangles;
   int mode;
{
   int i;
   GC gc;

   gc = mode ? DissolveOutGC : DissolveInGC;

   for (i = 0 ; i < NUM_DISSOLVE_STEPS ; i++) {
      XSetStipple(dpy, gc, DissolvePixmaps[i]);
      if (mode)
	 XFillRectangles(dpy, drawable, gc, rectangles, nrectangles);
      else
	 XDrawRectangles(dpy, drawable, gc, rectangles, nrectangles);
      XFlush(dpy);
      usleep(50000);
   }
}

DissolveSegments(dpy, drawable, segments, nsegments, mode)
   Display *dpy;
   Window drawable;
   XSegment segments[];
   int nsegments;
   int mode;
{
   int i;
   GC *gc;

   gc = mode ? DissolveOutLineGC : DissolveInLineGC;

   for (i = 0 ; i < NUM_LINE_STEPS ; i++) {
      XDrawSegments(dpy, drawable, gc[i], segments, nsegments);
      XFlush(dpy);
      usleep(50000);
   }
}

DissolveTree(dpy, drawable, rectangles, nrectangles, segments, nsegments, mode)
   Display *dpy;
   Window drawable;
   XRectangle rectangles[];
   int nrectangles;
   XSegment segments[];
   int nsegments;
   int mode;
{
   int i;
   int j = 0;
   int idle;
   GC gc;
   GC *lineGC;

   gc = mode ? DissolveOutGC : DissolveInGC;
   lineGC = mode ? DissolveOutLineGC : DissolveInLineGC;

   /* speed up if there are lots of nodes */
   idle = nrectangles > 50 ? 0 : 25000;
   
   for (i = 0 ; i < NUM_DISSOLVE_STEPS ; i++) {
      XSetStipple(dpy, gc, DissolvePixmaps[i]);
      if (mode)
	 XFillRectangles(dpy, drawable, gc, rectangles, nrectangles);
      else
	 XDrawRectangles(dpy, drawable, gc, rectangles, nrectangles);
      if (i % 2)
	 XDrawSegments(dpy, drawable, lineGC[j++], segments, nsegments);
      XFlush(dpy);
      usleep(idle);
   }
}

DissolvePolygon(dpy, drawable, pts, num_pts, mode)
   Display *dpy;
   Window drawable;
   XPoint *pts;
   int num_pts;
   int mode;
{
   int i;
   GC gc;

   gc = mode ? DissolveOutGC : DissolveInGC;

   for (i = 0 ; i < NUM_DISSOLVE_STEPS ; i++) {
      XSetStipple(dpy, gc, DissolvePixmaps[i]);
      XFillPolygon(dpy, drawable, gc, pts, num_pts,
		   Nonconvex, CoordModeOrigin);
      XFlush(dpy);
      usleep(50000);
   }
}

