//
//	TreeButton.m -- a class to attach to tree data structures
//		This class requires the String class, also by Don Yacktman.
//		Written by Don Yacktman (c) 1993 by Don Yacktman.
//				All rights reserved.
//
//	I doubt this will be useful for much beyond what it does in the demo
//		program supplied in this directory.
//
//		You may use and copy this class freely as long as you
//		comply with the following terms:
//			(1) If you use this class in an application which you
//				intend to sell commercially, as shareware, or otherwise,
//				you may only do so with express written permission
//				of the author.  Use in applications which will
//				be distributed free of charge is encouraged.
//			(2) You must include the source code to this object and
//				all accompanying documentation with your application,
//				or provide it to users if requested, free of charge.
//			(3) Do not remove the author's name or any of the
//				copyright notices
//

#import "TreeButton.h"
#import "TreeButtonCell.h"
#import "NamedTree.h"

@implementation TreeButton

static id treeButtonCell;

+ initialize
{
	if (self == [TreeButton class]) {
		treeButtonCell = [TreeButtonCell class]; // default cell class
	}
	return self;
}

+ setCellClass:classId
{
	treeButtonCell = classId;
	return self;
}

- initFrame:(const NXRect *)frameRect
{
	id oldCell;
	
	[super initFrame:frameRect];
	oldCell = [self setCell:[[[treeButtonCell alloc] init] setParent:self]];
	[oldCell free];
	
	return self;
}

- wasSelected:sender
{	// message is sent here by the Cell if the user clicked the button
	if ([myTreeNode respondsTo:@selector(activateNode:)])
		[myTreeNode activateNode:self];
	return self;
}

- treeNode { return myTreeNode; }
- setTreeNode:node { myTreeNode = node; return self; }

@end
