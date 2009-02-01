
#import <appkit/appkit.h>

@interface TreeController:Object
{
    id	treeView;
    id  infoPanel;
    int nextX, nextY;
    BOOL first;
}

- init;
- info:sender;
- open:sender;
- (BOOL)openFile:(const char *)name;

/* Application delegate methods */

- appDidInit:(Application *)sender;
- (int)app:sender openFile:(const char *)file type:(const char *)type;
- (BOOL)appAcceptsAnotherFile:sender;

@end
