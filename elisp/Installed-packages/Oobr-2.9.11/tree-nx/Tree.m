//
//	Tree.m -- a generic class to build tree data structures
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

#import "Tree.h"

@implementation Tree

- init			// special init for this class
{
	[self initLabel:""];
	return self;
}

- initLabel:(const char *)newLabel
{
	[super init];
	notCollapsed = YES;
	// set up a list class to hold pointers to other Tree objects (branches).
	branches = [[List alloc] init];	// make a dynamic-sized array of objects
	label = [[String alloc] init];	// make a string for the label
	[label setString:newLabel];
	return self;
}

- initLabelString:string	// send in a String object
{
	[self init];
	if (label) [label free];
	label = string;
	return self;
}

- setLabel:(const char *)newLabel
{
	[label setString:newLabel];
	return self;
}

- setValue:newValue
{
	if (value) [value free];
	value = newValue;
	return self;
}

- free			// clean up our mess
{
	[branches freeObjects];	// free the branches we're responsible for
	// (I assumed that each branch only has one parent...if that's not
	// the case, then the above line should be axed.)
	[branches free];		// get rid of the List object
	return [super free];
}

- addBranch:child	// add a new child node
{	// add to the end of the "branches" List object
	return [branches addObject:child];
}

- dumpTree:(NXStream *)file level:(int)lev indent:(const char *)ind
{
	int i; BOOL doKids;
	
	for (i=0; i<lev; i++) NXPrintf(file, "%s", ind);	// indent
	NXPrintf(file, "%s", [label stringValue]);		// and print label
	doKids = [self moreData:file level:lev indent:ind];
	NXPrintf(file, "\n");		       			// print newline
	if (doKids)
		for (i=0; i<[branches count]; i++)		// then do children
			[[branches objectAt:i]
			 dumpTree:file level:(lev + 1) indent:ind];
	return self;
}

- (BOOL)moreData:(NXStream *)file level:(int)lev indent:(const char *)ind
{	// Subclass responsibility -- you can dynamically control collapsing
	// (for example, cut off at a certain level, etc.) and also add info
	// to the end of a dumped node's line from here.  Be sure to message
	// super when you override this method; if this method returns a NO
	// then you should return a NO, regardless.  Don't just use the
	// notCollapsed instance var, since it may change in the future; look
	// at the return value from super!
	//
	// Here's how you might override to keep from printing levels deeper
	// than level 2 (remember that the root level is zero):
	//
	// - (BOOL)moreData:(NXStream *)file level:(int)lev indent:(const char *)ind
	// {
	//    if ((lev > 2) || ![super moreData:file level:lev indent:ind])
	//	 return NO;
	//    return YES;
	// }
	//
	return notCollapsed;
}

- (int)width
{
	int num = [branches count];
	int i, count = 0;
	
	if (!num) return 1;	// No children, so we're only one node wide...
	
	// have kids, so sum up their widths.
	for (i=0; i<num; i++) count += [[branches objectAt:i] width];
	return count;
}

- (int)depth
{
	int num = [branches count];
	int i, temp, deepest = 1;
	
	if (!num) return 1;	// No children, so only one node deep
	
	// have kids, so see which branch is deepest.
	for (i=0; i<num; i++) {
		temp = [[branches objectAt:i] depth];
		if (temp > deepest) deepest = temp;
	}
	return (deepest + 1);	// we are one deeper than the deepest child.
}

- branches { return branches; }
- collapse { notCollapsed = NO; return self; }
- uncollapse { notCollapsed = YES; return self; }
- (BOOL)collapsed { return !notCollapsed; }
- (const char *)label { return [label stringValue]; }
- (const char *)value { return [value stringValue]; }

@end
