/* ----------------------------------------------------------------------------
 *     Double buffering code
 * ----------------------------------------------------------------------------
 */


#include "dbl.h"

struct {                                               
    unsigned short red;                                        
    unsigned short green;
    unsigned short blue;
} color[] = {
    { 65280, 65280, 65280 }, /* white  */                      
    {     0,     0, 65280 }, /* blue   */
    {     0, 65280,     0 }, /* green  */
    { 65280,     0,     0 }, /* red    */
    { 42240, 10752, 10752 }, /* brown  */
    { 65280, 32512,     0 }, /* orange */
    { 32512, 32512, 32512 }, /* gray   */
    {     0,     0,     0 }  /* black  */
};                       

/* ------------------------------------------------------------------------- */

DoubleBuffer *
DBLcreate_double_buffer (display, window, backing_store, colors, num_colors)
   Display *display;
   Window   window;
   int      backing_store;
   XColor  *colors;
   int      num_colors;
{                                                                
   int i, j, k, l, m, offset, mask, size;
   int max_planes;

   char         *string;
   Surface     *surface;
   DoubleBuffer     *db;
   XGCValues       xgcv;
   unsigned long   xgcvmask;

/* allocate the double buffer structure,  and then open the display */

   if ((db = (DoubleBuffer *)calloc(1, sizeof(DoubleBuffer))) == 0) {
      printf("DBLopen_double_buffer : memory allocation error\n");
      return (NULL);
   }                                                          

/* note the display */

   db->display  = display;

/* first some information about our display */

   db->screen   = DefaultScreenOfDisplay(db->display);
   db->window   = window;

/* now get some information on color resources */

   db->visual   = DefaultVisualOfScreen(db->screen);
   db->depth    = DefaultDepthOfScreen(db->screen);
   db->colormap = DefaultColormapOfScreen(db->screen);

/* set up colors */

   for (i = 0 ; i < num_colors; i++) {
      color[i].red   = colors[i].red;
      color[i].green = colors[i].green;
      color[i].blue  = colors[i].blue;
   }
   
/* see if the user wanted to limit the number of planes used
   then see how many are available,  make it a multiple of 2 */

   if ((string = getenv("DBL_MAX_PLANES")) == NULL)
      max_planes = DBL_MAX_PLANES;
   else {
      max_planes = atoi(string);
   }

   if ((db->num_planes = PlanesOfScreen(db->screen)) > max_planes) {
      db->num_planes = max_planes;
   }

   db->num_planes = (db->num_planes >> 1) << 1;


/* otherwise allocate contiguous planes to do double buffering */

   while (db->num_planes >= DBL_MIN_PLANES) {
      if (XAllocColorCells (db->display, db->colormap, 1, db->planes,
			    db->num_planes, db->pixels, 1)) {
	 break;
      }

      db->num_planes -= 2;
   }

/* if we have at least minimum planes, then we can do double
   buffering and we want to setup our surfaces and colormaps */

   if (db->num_planes < DBL_MIN_PLANES)
      db->num_surfaces = 0;
   else {
      db->num_colors   = 1 << (db->num_planes >> 1);
      db->num_surfaces = DBL_MAX_SURFACES;

   /* if the number of colors is less than DBL_MAX_COLORS,
      then we want to make sure black is the  last  color */

      for (i = db->num_colors - 1; i < DBL_MAX_COLORS; i++) {
         color[i].red   = color[DBL_MAX_COLORS - 1].red;
         color[i].green = color[DBL_MAX_COLORS - 1].green;
         color[i].blue  = color[DBL_MAX_COLORS - 1].blue;
      }

   /* we have a set of contiguous planes.  compute a mask for
      the planes,  and figure out the offset in the  hardware */

      for (i = 0; i < db->num_planes; i++) {
         db->mask |= db->planes[i];
      }

      mask   = db->mask;
      offset = 0;

      while ((mask & 1) == 0) {
         mask = mask >> 1;
         offset = offset + 1;
      }

      mask = (1 << (db->num_planes >> 1)) - 1;                             

   /* now create the surfaces that will contain plane mask and
      colormap information that we use to do double  buffering */

      for (i = 0; i < db->num_surfaces; i++) {
         size = sizeof(Surface) + sizeof(XColor) * (1 << db->num_planes);

         if ((surface = (Surface *)malloc(size)) != NULL)
            db->surface[i] = surface;
         else {
            printf("DBLcreate_double_buffer : memory allocation error\n");
            DBLdelete_double_buffer(db);
            return(NULL);
         }

         surface->offset     = offset + i * (db->num_planes >> 1);
         surface->mask       = mask << surface->offset;
         surface->num_colors = 1 << db->num_planes;
  
      /* compute our pixel values by taking every permutation
         of the pixel and planes returned by XAllocColorCells */

         for (j = 0; j < (surface->num_colors); j++) {
            surface->color[j].pixel = db->pixels[0];
         }

         for (j = 0; j < db->num_planes; j++) {
            for (k = (1 << j); k < (surface->num_colors); k += (2 << j)) {
               for (l = k; l < (k + (1 << j)); l++) {                  
                  surface->color[l].pixel |= db->planes[j];
               }
            }
         }

       /* now populate those pixels with the proper  colors  so             
          that we can do animation by banging in a new colormap */

         for (j = 0; j < surface->num_colors; j++) {
            k = (j & surface->mask) >> surface->offset;

            surface->color[j].red   = color[k].red;                 
            surface->color[j].green = color[k].green;
            surface->color[j].blue  = color[k].blue;

            surface->color[j].flags = DoRed | DoGreen | DoBlue;
         }
      }

      db->current_surface = 0;
   }


/* now figure out what pixel values we will use to draw with
   and store them in the double buffer structure */

   if (db->num_surfaces == 0) {
      db->num_colors = DBL_MAX_COLORS;
      db->colors[0]  = WhitePixelOfScreen(db->screen);

      for (i = 1; i < db->num_colors; i++) {
	 db->colors[i] = BlackPixelOfScreen(db->screen);
      }
   }
   else {
      for (i = 0; i < db->num_colors; i++) {
         j = (i << (db->num_planes >> 1)) + i;
         db->colors[i] = db->surface[0]->color[j].pixel;
      }
   }

/* fill out the remaining colors with the last color */

   for (; i < DBL_MAX_COLORS; i++) {
      db->colors[i] = db->colors[db->num_colors - 1];
   }

   db->width   = WidthOfScreen(db->screen);
   db->height  = HeightOfScreen(db->screen);

/* if there are no surfaces then we are doing animation with
   a frame buffer,  so create a pixmap as our frame buffer   */
                         
   if (db->num_surfaces > 0)
      db->drawable = db->window;   
   else {
      db->frame = XCreatePixmap(db->display, db->window,
				db->width, db->height, db->depth);
      db->drawable = db->frame;
   }                                                                    

/* if they have requested backing store,  then create an extra
   pixmap which we can use as backing store to handle exposures */

   if (backing_store) {
      db->backing = XCreatePixmap(db->display, db->window,
				  db->width, db->height, db->depth);
   }                                                   

/*  use the 0 pixel from one of the surfaces for the background */

   xgcv.background = DBLinq_background(db);
   xgcv.line_style = LineSolid;
   xgcv.line_width = 0;
   xgcv.cap_style  = CapButt;
   xgcv.join_style = JoinRound;
   xgcvmask = GCBackground | GCLineStyle | GCLineWidth | GCCapStyle | 
              GCJoinStyle;
   
   db->gc = XCreateGC(db->display, db->drawable, xgcvmask, &xgcv);

/* do an initial frame to setup the colormap,  and return */

   DBLbegin_frame(db);
   DBLend_frame(db, 1);
   return (db);
}

/* ------------------------------------------------------------------------- */

void
DBLdelete_double_buffer (db)
     DoubleBuffer *db;
{
  int i;

  /* remove and and all surfaces that are out there */
  
  for (i = 0; i < DBL_MAX_SURFACES; i++) {
    if (db->surface[i] != 0) {
      free(db->surface[i]);                               
    }
  }

  /* now clean up the various resources used for this double buffer */
  
  if (db->frame != 0) {
    XFreePixmap(db->display, db->frame);
  }
  
  if (db->backing != 0) {
    XFreePixmap(db->display, db->backing);
  }
  
  /* if we created our own private colormap,  then free the colormap */
  
  if (db->colormap != DefaultColormapOfScreen(db->screen)) {
    XFreeColormap(db->display, db->colormap);
  }
  
  free (db);
}

/* ------------------------------------------------------------------------- */

unsigned long
DBLinq_background(db)
   DoubleBuffer *db;
{
   if (db->num_surfaces > 0)
      return(db->surface[0]->color[0].pixel);
   else 
      return(WhitePixelOfScreen(db->screen));
}

/* ------------------------------------------------------------------------- */

DBLbegin_frame(db)
     DoubleBuffer *db;
{
  Surface   *surface;
  
  /* there will be at most two surfaces optimize with "&"*/
  
  if (db->num_surfaces > 0) {                       
    db->current_surface = (db->current_surface + 1) & 1;                  
    surface = db->surface[db->current_surface];
  }
   
  /* clear the back surface of the window which may actually be a pixmap */ 
  
  if (db->num_surfaces > 0) 
    XSetPlaneMask (db->display, db->gc, surface->mask);
  
  /* clear out the back surface or frame buffer as appropriate */
  
  XSetFunction(db->display, db->gc, GXclear);
  XFillRectangle(db->display, db->drawable, db->gc,
		 0, 0, db->width, db->height);
  
  /* set writing mode back to copy */
  XSetFunction (db->display, db->gc, GXcopy);
  
  XSync(db->display, False);
}  
         

/* ------------------------------------------------------------------------- */
                                            

DBLend_frame(db, init)
     DoubleBuffer *db;                                                   
     short init;
{
  Surface  *surface;
  
  /* if there are no drawing surfaces,  then we are doing animation   
     with a frame buffer, copy the frame buffers to their viewports */

  if (db->num_surfaces == 0) {
     if (! init)
        XCopyArea (db->display, db->frame, db->window,
	           db->gc, 0,0, db->width, db->height, 0,0);
  } else {
    
    /* otherwise,  we can flip the surface by banging in the new colormap */

    XSync(db->display, False);
    surface = db->surface[db->current_surface];
    XStoreColors (db->display, db->colormap,
		  surface->color, surface->num_colors);
  }

  if (db->backing != 0) {
    XCopyArea (db->display, db->window, db->backing,
	       db->gc, 0,0, db->width, db->height, 0,0);
  }
  
  /* make sure this all goes off to the server,  right away */
  
  XSync(db->display, False);
}













