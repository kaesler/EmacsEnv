//
//	Tree.h -- a generic class to build tree data structures
//		This class requires the String class, also by Don Yacktman.
//		Written by Don Yacktman (c) 1993 by Don Yacktman.
//				All rights reserved.
//
//	Subclasses should be designed to hold more data than just children and
// 		a String-based label...That's where the usefulness of the class
//		becomes apparent.  By using a list, any number of children is ok.
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

@interface Tree:Object
{
	id branches;	// an instance of the list class
	id label;	// node name
	id value;       // must be a string
	BOOL notCollapsed;	// print children when dumping if true.
}


// init with null label
- init;

// designated initializer
- initLabel:(const char *)newLabel;	// send a char* string
- initLabelString:string;		// send a String object

// access to the label of this node
- setLabel:(const char *)newLabel;      // send a char* string
- (const char *)label;

// access to the value of this node
- setValue:newValue;                    // send a String object
- (const char *)value;

// clean up our mess
- free;

// add a new child node
- addBranch:child;

// Print the tree to a stream (file, whatever).  Call the root with level
// set to zero, and set the indent string however you like; the indent
// string should be something like "  " or "\t" to show how to indent to
// the next level.  This method recursively traverses the tree's children.
- dumpTree:(NXStream *)file level:(int)lev indent:(const char *)ind;

// set whether or not we print the children (we don't if collapsed)
// when dumping.  This does NOT affect the tree's width or depth!
- collapse;
- uncollapse;
- (BOOL)collapsed;

// when dumping the tree, if you want to add extra data to the output
// before the newline and before children are printed, add it here.  If
// you return NO, then the children won't be printed; this is how the
// collapse stuff works.
- (BOOL)moreData:(NXStream *)file level:(int)lev indent:(const char *)ind;

// How deep or wide is the tree?
- (int) width;
- (int) depth;

// Return the List object that contains ids of all the kids.
- branches;

@end
