
#import "TreeView.h"
#import "TreeButton.h"
#import "NamedTree.h"
#import "Line.h"

// constants to determine how the buttons are laid out
#define BUTTONWIDTH	155.0
#define BUTTONHEIGHT	 24.0
#define VERTSPACING 	  8.0
#define HORIZSPACING	 40.0

@implementation TreeView

- initFrame:(const NXRect *)frameRect
{
  [super initFrame:frameRect];
  [self setAutosizing:(unsigned int) (NX_WIDTHSIZABLE | NX_HEIGHTSIZABLE)];

  currentButton = nil;
  lineList = nil;
  priorButton = nil;
  [selectedField setNextText: selectedField];
  [selectedField setPreviousText: selectedField];
  currScale  = 1.0;

  [self registerForDragging];
  [self setOpaque:YES];

  return self;
}

- buildTreeFromNode:aNode bottom:(double)ybot
		top:(double)ytop atX:(double)xpos parent:(NXPoint *)pos
{	// add a button representing the node to the View
	// This method is recursive.
	NXRect butFrame = {{(xpos + HORIZSPACING),
			(ybot + (ytop - ybot) / 2 - BUTTONHEIGHT / 2)},
			{BUTTONWIDTH, BUTTONHEIGHT}};
	id newButton = [[[TreeButton alloc] initFrame:&butFrame]
			setTreeNode:aNode];
	id kid, kids = [aNode branches];
	int numBranches = [kids count];
	int i, treeWidth; double diff, accum = ybot;
	NXPoint myCenter = {(NX_X(&butFrame)),
			    (NX_Y(&butFrame) + BUTTONHEIGHT / 2)};
	id newLine;
	
	[newButton setTitle:[aNode label]];
	[self addSubview:newButton];
	// line to parent:
	if (pos) {	// NULL if root, so no line
		NXPoint parentRight = { pos->x + BUTTONWIDTH, pos->y };
		newLine = [[Line alloc] init];
		[newLine setStart:&parentRight end:&myCenter];
		[lineList addObject:newLine];
	}
	// now add any children and the lines to them.
	for (i=numBranches - 1; i >= 0; i--) { // loop isn't entered if no kids.
		kid = [kids objectAt:i];
		treeWidth = [kid width];
		diff = (treeWidth * (BUTTONHEIGHT + VERTSPACING));
		[self buildTreeFromNode:kid bottom:accum
				top:(accum + diff + VERTSPACING)
				atX:(xpos + BUTTONWIDTH + HORIZSPACING)
				parent:&myCenter];
		accum += diff;
	}
	return self;
}

- attachTree:aTree
{
	int treeWidth = [aTree width];
	int treeDepth = [aTree depth];
	double height = (treeWidth * (BUTTONHEIGHT + VERTSPACING) + VERTSPACING);
	double width  = (treeDepth * (BUTTONWIDTH + HORIZSPACING) + HORIZSPACING);
	
	treeRoot = aTree;
	if (lineList) [[lineList freeObjects] free];
	lineList = [[List alloc] init];
	// resize the View to accomodate the Buttons
	[self sizeTo:width :height];
	[self buildTreeFromNode:aTree bottom:0.0 top:height atX:0.0 parent:NULL];

	return self;
}

- drawSelf:(NXRect *)rects :(int)rectCount  	// standard rendering method
{
    NXColor color = [[self window] backgroundColor];

    if (NXEqualColor(color, NX_COLORLTGRAY))
       color = NX_COLORDKGRAY;

    // PSsetgray(NX_DKGRAY);
    NXSetColor(color);
    NXRectFill(&bounds);
    // PSsetgray(NX_BLACK);
    NXSetColor(NX_COLORBLACK);
    PSsetlinewidth(2.0);

    [lineList makeObjectsPerform:@selector(render)];
    [[self subviews] makeObjectsPerform:@selector(display)];
    return self;
}

- scale:sender
{
  id popUp = [sender window];
  short index = [popUp indexOfItem:[popUp selectedItem]];
  //                   25%   50%  75%  100%  125%  150%  200%  SizeToFit
  //                   0     1      2     3     4     5     6    7
  float factors[] = {0.25,  0.50, 0.75,  1.0, 1.25, 1.50, 2.0, 0.20};
  NXPoint center;
  NXCoord scale = factors[index];

  // Initialize width and height bounds when view is not scaled.
  if (currScale == 1.0)
    {
      origWidth = NX_WIDTH(&bounds);
      origHeight = NX_HEIGHT(&bounds);
    }

  // Remember the center to we can reset it after the scaling.
  center.x = NX_X(&bounds) + NX_WIDTH(&bounds) / 2;
  center.y = NX_Y(&bounds) + NX_HEIGHT(&bounds) / 2;

  // Scale the view to its new size
  if (index == 3) // 100% (Normal Size)
    {
     [self setDrawSize:origWidth :origHeight];
     currScale  = 1.0;
    }
  else
    {
      currScale *= scale;
      [self setDrawSize:NX_WIDTH(&bounds) / currScale
                       :NX_HEIGHT(&bounds) / currScale];
    }

  // Reset the center point
  [self setDrawOrigin:center.x - NX_WIDTH(&bounds) / 2
	 	     :center.y - NX_HEIGHT(&bounds) / 2];

  // Ensure that selected button, if any, is visible.
  [self displayBut:currentButton];

  [self update];

  return self;
}

- setCurrentButton:but
{
  if (but)
    {
      priorButton = currentButton;
      if (priorButton)
	{
	  [priorButton setType:NX_MOMENTARYPUSH];
	  [priorButton setState:0];
	}
      currentButton = but;
      [currentButton setType:NX_ONOFF]; [currentButton setState:1];
      // [selectedField setStringValueNoCopy: [but title]];
    }
  return but;
}

- setCurrButtonByName:sender
{
  id currBut = [self getButByName:[sender stringValue]];

  [self displayBut:[self setCurrentButton:currBut]];
  [treeRoot act:currBut];
  return currBut;
}

- getButByName:(const char*)name
{
  id butList = [self subviews];
  id but = nil;
  id currBut = nil;
  int i = 0;

  while (!currBut && (but = [butList objectAt:i++]))
    {
      if (!strcmp([but title], name))
	currBut = but;
    }
  return currBut;
}

- displayButByName:sender
{
  id but = [self getButByName:[sender stringValue]];

  if (but)
    [self displayBut:but];
  return but;
}

- displayBut:but
{
  NXRect butRect;

  if (but)
    {
      [[but getBounds:&butRect] convertRectToSuperview:&butRect];
      [self scrollRectToVisible:&butRect];
    }
  return self;
}

@end



// Color dragging support

BOOL includesType(const NXAtom *types, NXAtom type)
{
    if (types)
      while (*types)
	if (*types++ == type)
	  return YES;
    return NO;
}

@implementation TreeView(Drag)

- registerForDragging
{
 [self registerForDraggedTypes:&NXColorPboardType count:1];
 return self;
}

- (NXDragOperation)draggingEntered:(id <NXDraggingInfo>)sender
{
  NXDragOperation sourceMask = [sender draggingSourceOperationMask];
  Pasteboard *pboard = [sender draggingPasteboard];

  return ((includesType([pboard types], NXColorPboardType))
	   ? NX_DragOperationGeneric : NX_DragOperationNone);
}

- (BOOL)prepareForDragOperation:(id <NXDraggingInfo>)sender
{
  return YES;
}

- (BOOL)performDragOperation:(id <NXDraggingInfo>)sender
{
    Pasteboard *pboard = [sender draggingPasteboard];

    if (includesType([pboard types], NXColorPboardType))
      {
	NXColor color = NXReadColorFromPasteboard(pboard);
	[[self window] setBackgroundColor:color];
	[self display];  // reflect color change
	return YES;
      }
    else
      return NO;
}

- concludeDragOperation:(id <NXDraggingInfo>)sender
{
  // Return value ignored.
  return nil;
}

@end
