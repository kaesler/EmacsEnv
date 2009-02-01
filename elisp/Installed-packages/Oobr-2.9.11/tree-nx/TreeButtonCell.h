
#import <appkit/appkit.h>

@interface TreeButtonCell:ButtonCell
{
	id parent;
}

- parent;
- setParent:anObject;
- (BOOL)trackMouse:(NXEvent *)theEvent
		inRect:(const NXRect *)cellFrame ofView:aView;
- performClick:sender;

@end
