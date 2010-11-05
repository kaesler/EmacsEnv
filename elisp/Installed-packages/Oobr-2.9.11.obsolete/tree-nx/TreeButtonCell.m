
#import "TreeButtonCell.h"
#import "TreeButton.h"

@implementation TreeButtonCell

- parent { return parent; }
- setParent:anObject
{	// anObject should be the parent Control object subclass
	parent = anObject;
	return self;
}

- (BOOL)trackMouse:(NXEvent *)theEvent
		inRect:(const NXRect *)cellFrame ofView:aView
{	// this traps the button being selected.
	BOOL result = [super trackMouse:theEvent inRect:cellFrame ofView:aView];
	if (result && [parent respondsTo:@selector(wasSelected:)]) {
		[parent wasSelected:self];
	}
	return result;
}

- performClick:sender
{
	[super performClick:sender];
	if ([parent respondsTo:@selector(wasSelected:)]) {
		[parent wasSelected:self];
	}
	return self;
}

@end
