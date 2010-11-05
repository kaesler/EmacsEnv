
#import "Line.h"

@implementation Line

- setStart:(NXPoint *)s end:(NXPoint *)e
{
	start.x = s->x;
	start.y = s->y;
	end.x = e->x;
	end.y = e->y;
	return self;
}

- render
{
	PSmoveto(start.x, start.y);
	PSlineto(end.x, end.y);
	PSstroke();
	return self;
}

@end
