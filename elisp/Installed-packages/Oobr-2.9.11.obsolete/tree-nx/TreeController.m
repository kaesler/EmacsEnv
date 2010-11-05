
#import <stdio.h>

#import "TreeController.h"
#import "NamedTree.h"
#import "TreeView.h"

@implementation TreeController

- appDidInit:(Application *)sender
{
  BOOL haveOpenedDocument = NO;	// whether we have opened a document

//  // Gets the public port for SomeApp
//  port_t  thePort = NXPortFromName("Emacs", NULL); 
//
//  if (thePort != PORT_NULL)
//    // Sets the Speaker to send its next message to SomeApp's port
//    [[NXApp appSpeaker] setSendPort:thePort]; 

  if (NXArgc > 1)
  {
     int i;

     for (i = 1; i < NXArgc; i++)
     {
	haveOpenedDocument = [self openFile: NXArgv[i]] || haveOpenedDocument;
     }
  }

    return self;
}

- init
{
	[super init];
    first = YES;
    nextX = 200;
    nextY = 600;
      return self;
}

- info:sender // bring up the info panel, obviously
{
	if(!infoPanel)
		[NXApp loadNibSection:"InfoPanel.nib" owner:self withNames:NO];
	return [infoPanel orderFront:sender];
}

- open:sender
{	// use open panel to select a file -- only with .tree extension.
	// only one file may be loaded at a time.
	const char *const *files;
	char *file;
	const char *dir;
	static const char *const ft[2] = {"tree", NULL};

	id openPanel = [OpenPanel new];
	[openPanel allowMultipleFiles:NO];
	if (first) {
		[openPanel runModalForDirectory:[[NXBundle mainBundle] directory]
				file:NULL types:ft];
		first = NO;
	} else  [openPanel runModalForTypes:ft];
	files = [openPanel filenames];
	dir = [openPanel directory];
	file = malloc(strlen(files[0]) + strlen(dir) + 8);
	strcpy(file, dir);
	strcat(file,"/");
	strcat(file, files[0]);
	strcat(file, "\0");
	[self openFile:file];
	return self;
}

char nextChar; // This allows me to do single character lookahead in scan

id readToNewline(FILE *file) // used to parse a file; reads until a newline
{	// returns a string object... reads until EOL to get string value.
	id newString = [[String alloc] init];
	char *buffer = (char *)malloc(1024);	// should be plenty big
	char *spot = buffer;

	while (nextChar != '\n')
	  {
	    spot[0] = nextChar; spot++; nextChar = fgetc(file);
	  }	
	spot[0] = '\0'; // terminate the string
	nextChar = fgetc(file);  // Get next char for next invocation of this function
	[newString setString:buffer];
	free(buffer);
	return newString;
}

char prevChar; // This allows me to do single character lookback in scan

id readToSep(FILE *file) // used to parse a file; reads until a newline or a "^^" sequence
{	// returns a string object... reads until EOL to get string value.
	id newString = [[String alloc] init];
	char *buffer = (char *)malloc(1024);	// should be plenty big
	char *spot = buffer;
	int c;

	while (nextChar != '\n')
	  {
	    if (nextChar == '^')
	      if ((c = fgetc(file)) == '^')
		break;
	      else
		ungetc(c, file);
	    spot[0] = nextChar; spot++; nextChar = fgetc(file);
	  }	
	spot[0] = '\0'; // terminate the string
	prevChar = nextChar;
	nextChar = fgetc(file);  // Get next char for next invocation of this function
	[newString setString:buffer];
	free(buffer);
	return newString;
}

// This actually opens a file.  WorkSpace and Open panel methods both
// eventually get to here to do the real work.  This code is pretty much
// worth ignoring, unless you _really_ care about how I read in the
// files.  It's mostly specific to the file format so it's not
// generally useful.  The framework for this code came from the
// code in my "Viewer.app" that is in with some PD raytracers I ported
// to the NeXT--allows viewing an rgb bitmap file; I wrote it before
// GW and ImageViewer existed...  (See raytracers.tar.Z on sonata/orst)
- (BOOL)openFile:(const char *)name
{
	// id alert;
	id aString, treeName, rootNode, workingNode, tempNode;
	id newString, stack = [[List alloc] init];
	int indLevel, numSpaces, indent = 0;
	char *tempString;
	BOOL rStat = YES;
	FILE *file;
	// for debugging:
	//NXStream *out = NXOpenFile(fileno(stdout), NX_WRITEONLY);

	// get a new doc window.
	[NXApp loadNibSection:"DocWindow.nib" owner:self withNames:NO];
	[[treeView window] setTitleAsFilename:name];
	// put up an alert panel to let user know we're busy
	// alert = NXGetAlertPanel(NULL, "Reading tree and creating image.",
	//		NULL, NULL, NULL);
	// [alert makeKeyAndOrderFront:self];
	// Read the tree file.  NOTE THAT THIS DOES NOT DO ERROR CHECKING.
	file = fopen(name, "r");
	nextChar = fgetc(file);	// prime the system
	treeName = readToNewline(file); // first line is tree's name
	aString = readToSep(file);	// Get the name of the root node.
	rootNode = [[[NamedTree alloc]
			initLabelString:aString] setTreeName:treeName];
	if (prevChar != '\n')
	  [rootNode setValue: readToSep(file)];  // Set the node's value.
	[stack insertObject:rootNode at:0];
	workingNode = rootNode;
	// figure out the indentation
	while (nextChar == ' ') {
		indent++;
		nextChar = fgetc(file);
	}
	aString = readToSep(file); // get name of child node
	tempNode = [[[NamedTree alloc]
			initLabelString:aString] setTreeName:treeName];
	if (prevChar != '\n')
	  [tempNode setValue: readToSep(file)];  // Set the node's value.
	[workingNode addBranch:tempNode];
	[stack insertObject:tempNode at:0];
	workingNode = tempNode;
	// now that we know the file's char's, we read in the other nodes
	// I use a List object as if it were a stack and push parent nodes on
	// it while working on children rather than doing a recursive function.
	// the comments are sparse, just know that it's mostly pushing and
	// popping from the stack to get at the right parent to add a child to.
	while (!feof(file)) {
		aString = readToSep(file); // next node name + indentation.
		// find out # of indentation spaces and strip them off
		// *** This gives a warning: ignore it, it's unimportant here.
		tempString = [aString stringValue]; numSpaces = 0;
		while (tempString[0] == ' ') {
			numSpaces++; tempString++;
		}
		indLevel = numSpaces / indent;
		if (indLevel == ([stack count] - 1)) // same level as last object
		  {
			[stack removeObjectAt:0];
			workingNode = [stack objectAt:0];
			newString = [[String alloc] initString:tempString];
			[aString free];
			tempNode = [[[NamedTree alloc]
					initLabelString:newString] setTreeName:treeName];
                        if (prevChar != '\n')
			  [tempNode setValue: readToSep(file)];  // Set the node's value.
			[workingNode addBranch:tempNode];
			[stack insertObject:tempNode at:0];
			workingNode = tempNode;
		} else if (indLevel == ([stack count])) { // child of last node
			newString = [[String alloc] initString:tempString];
			[aString free];
			tempNode = [[[NamedTree alloc]
					initLabelString:newString] setTreeName:treeName];
                        if (prevChar != '\n')
			  [tempNode setValue: readToSep(file)];  // Set the node's value.
			[workingNode addBranch:tempNode];
			[stack insertObject:tempNode at:0];
			workingNode = tempNode;
		} else if (indLevel < [stack count]) { // higher level, so pop
			while (indLevel < [stack count]) { // pop until at right level
				[stack removeObjectAt:0];
				workingNode = [stack objectAt:0];
			} // now add new node since we're at the level
			newString = [[String alloc] initString:tempString];
			[aString free];
			tempNode = [[[NamedTree alloc]
					initLabelString:newString] setTreeName:treeName];
                        if (prevChar != '\n')
			  [tempNode setValue: readToSep(file)];  // Set the node's value.
			[workingNode addBranch:tempNode];
			[stack insertObject:tempNode at:0];
			workingNode = tempNode;
		} else { // typically, if user goes in two levels at once, which
			// doesn't make any sense semantically
			fprintf(stderr, "Error: level too deep!\n");
			rStat = NO;
		}
	}
	// Debugging code to pretty print the parsed tree.  If this output
	// is correct, then we know that the parse was OK.
	//printf("\nHere's the parsed tree:\n");
	//printf("Tree name:  \"%s\".", [treeName stringValue]);
	//[rootNode dumpTree:out level:0 indent:"   "];
	//printf("\n\n");
	//NXClose(out);
	// end debug code
	// rStat = return status of tree reader  YES = success
	// Now attach the Tree to the TreeView...
	[treeView attachTree:rootNode];
	// and now bring up the window for the user
	[[treeView window] moveTo:nextX :(nextY-218)];
	// Get rid of the alert
	// [alert orderOut:self];
	// [alert free];
	nextX += 22; nextY -= 25;
	if (nextX > 370)
	{
		nextX = 200; nextY = 600;
	}
	[[treeView window] makeKeyAndOrderFront:self];
	return rStat;
}


// The next two methods allow the WorkSpace to open a .tree file when
// it is double-clicked.  (Or any file that's cmd-dragged over our icon.)

- (BOOL)appAcceptsAnotherFile:sender { return YES; }
- (int)app:sender openFile:(const char *)file type:(const char *)type
{
	return [self openFile:file];
}


@end
