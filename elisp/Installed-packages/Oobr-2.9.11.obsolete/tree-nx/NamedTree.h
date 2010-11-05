//
//	NamedTree.h -- a generic class to build tree data structures
//		This class requires the String class, also by Don Yacktman.
//		Written by Don Yacktman (c) 1993 by Don Yacktman.
//				All rights reserved.
//
//	This Tree subclass allows an entire tree to be given a name, other than
//		the root node's name.
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

#import "Tree.h" 	// superclass is in there

@interface NamedTree:Tree
{
	id treeName;	// a String object that all nodes should point to
}

// set and retrieve the tree's name
- setTreeName:string;
- (const char *)treeName;

- act:sender;            // action performed when node is activated
- activateNode:sender;	 // message sent to "activate" the node

@end
