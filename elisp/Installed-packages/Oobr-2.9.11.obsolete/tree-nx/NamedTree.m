//
//	NamedTree.m -- a generic class to build tree data structures
//		This class requires the String class, also by Don Yacktman.
//		Written by Don Yacktman (c) 1993 by Don Yacktman.
//				All rights reserved.
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

#import "NamedTree.h"
#import "TreeView.h"

@implementation NamedTree

- setTreeName:string { treeName = string; return self; }
- (const char *)treeName { return [treeName stringValue]; }

- activateNode:sender
{
//        int     msgDelivered;

	// this action message is sent whenever the button associated with
	// this node is clicked by the user.
	[[sender superview] setCurrentButton:sender];
        [self act:sender];
	return self;
}

- act:sender
{
	const char* nodeVal = [((value) ? value : label) stringValue];

	printf("%s^^%s^^%s\n", [self treeName], "br-edit", nodeVal);
//	msgDelivered = [[NXApp appSpeaker] selectorRPC:"nxBrowserMsg"
//			             paramTypes:"ccc",
//			             [self treeName], "br-edit", nodeVal];
//	if (msgDelivered != 0)
//	  fprintf(stderr, "Class display request not accepted.\n");

	return self;
}

@end
