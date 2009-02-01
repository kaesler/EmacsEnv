//
//	TreeButton.h -- a class to attach to tree data structures
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

#import <appkit/appkit.h> 	// superclass is in there
#import <stdio.h>
#import "String.h"

@interface TreeButton:Button
{
	id myTreeNode;	// this is the tree node we represent
}

+ initialize;
+ setCellClass:classId;
- initFrame:(const NXRect *)frameRect;
- wasSelected:sender;
- treeNode;
- setTreeNode:node;

@end

