/* ----------------------------------------------------------------------------
 *     Double buffering code
 * ----------------------------------------------------------------------------
 */

#include <stdio.h>
#include <X11/Xlib.h>

#define DBL_MAX_SURFACES 2
#define DBL_MIN_PLANES   2
#define DBL_MAX_PLANES   6
#define DBL_MAX_COLORS   (1 << (DBL_MAX_PLANES >> 1))

typedef struct _surface {
    int	         mask;          /* mask to use this surface           */
    int	       offset;          /* offset within colormap             */
    int	   num_colors;          /* number of colors in color array    */
    XColor   color[1];          /* the actual color array             */
} Surface;

typedef struct _doublebuffer {
    Display     *display;       /* X display for windows and pixmaps  */
    Screen      *screen;	/* X screen                           */
    Window	 window;	/* X window for this double buffer    */

    int		 width;		/* width of window                    */
    int          height;	/* height of window                   */

    Pixmap	 frame;	        /* pixmap for frame buffer            */
    Pixmap       backing;       /* pixmap for backing store           */
    Drawable     drawable;      /* copy of window/pixmap we draw in   */

    GC		 gc;		/* GC used to draw the drawable       */
    Visual      *visual;	/* X visual                           */
    Colormap     colormap;	/* X colormap identifier              */
    int          depth;	        /* depth of screen in planes          */
    int          num_planes;    /* number of planes used              */

/* surface information is used to do double buffering                 */ 

   int       num_surfaces;
   int       current_surface;
   Surface  *surface[DBL_MAX_SURFACES];

/* we need to remember which pixels and planes we allocated           */

   int       mask;
   long      pixels[DBL_MAX_COLORS];                                  
   long      planes[DBL_MAX_PLANES];

/* the pixel values one should use when drawing to the viewports      */

   int       num_colors; 
   int       colors[DBL_MAX_COLORS];
} DoubleBuffer;


extern DoubleBuffer  *DBLcreate_double_buffer();
extern void           DBLdelete_double_buffer();
extern unsigned long  DBLinq_background();
extern char          *getenv();





