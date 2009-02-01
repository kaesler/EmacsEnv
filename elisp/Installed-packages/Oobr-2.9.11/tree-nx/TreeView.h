
#import <appkit/appkit.h>
#import "TreeButton.h"

@interface TreeView:View
{
	id currentButton;
	id lineList;
	id priorButton;
	id selectedField;
	id treeRoot;

	NXCoord currScale;
	NXCoord origWidth;
	NXCoord origHeight;
}

- attachTree:aTree;
- buildTreeFromNode:aNode bottom:(double)ybot
		top:(double)ytop atX:(double)xpos parent:(NXPoint *)pos;
- displayBut:but;
- displayButByName:sender;
- drawSelf:(NXRect *)rects :(int)rectCount;  	// standard rendering method
- getButByName:(const char*)name;
- initFrame:(const NXRect *)frameRect;
- scale:sender;
- setCurrentButton:but;
- setCurrButtonByName:sender;

@end

/* Color Dragging */

@interface TreeView(Drag)

- registerForDragging;
- (NXDragOperation)draggingEntered:(id <NXDraggingInfo>)sender;
- (BOOL)prepareForDragOperation:(id <NXDraggingInfo>)sender;
- (BOOL)performDragOperation:(id <NXDraggingInfo>)sender;
- concludeDragOperation:(id <NXDraggingInfo>)sender;

@end
