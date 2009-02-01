
#import <appkit/appkit.h>

@interface Line:Object
{
	NXPoint start;
	NXPoint end;
}

- setStart:(NXPoint *)s end:(NXPoint *)e;
- render;

@end
