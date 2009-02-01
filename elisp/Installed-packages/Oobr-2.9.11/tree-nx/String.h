//
//	String.h -- a generic class to simplify manipulation of (char *)'s
//		Written by Don Yacktman (c) 1993 by Don Yacktman.
//				Version 1.1.  All rights reserved.
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

#import <appkit/appkit.h>

@interface String:Object
{
	 char *buffer;
	 NXStringOrderTable *orderTable;
	 int length, _length;
}

// basic allocation, deallocation methods
- init;
- initString:(const char *)aString;
- allocateBuffer:(int)size;
- allocateBuffer:(int)size fromZone:(NXZone *)zone;
- read:(NXTypedStream *)stream;
- write:(NXTypedStream *)stream;
- freeString;
- free;

// strcpy(), strlen() covers 
- copyFromZone:(NXZone *)zone; // a -copy message calls this.
- setString:(const char *)aString;
- setString:(const char *)aString fromZone:(NXZone *)zone;
- setStringValue:sender;
- setStringValue:sender fromZone:(NXZone *)zone;
- (const char *)stringValue;
- (int)length;

// strcat(), strncat() covers
- concatenate:sender;
- concatenate:sender n:(int)n;
- concatenate:sender fromZone:(NXZone *)zone;
- concatenate:sender n:(int)n fromZone:(NXZone *)zone;
- cat:(const char *)aString;
- cat:(const char *)aString n:(int)n;
- cat:(const char *)aString fromZone:(NXZone *)zone;
- cat:(const char *)aString n:(int)n fromZone:(NXZone *)zone;

// index(), rindex() covers
- (const char *)rindex:(char)aChar;
- (const char *)index:(char)aChar;

// strcmp(), strncmp(), strcasecmp(), strncasecmp() covers
- setStringOrderTable:(NXStringOrderTable *)table;
- (NXStringOrderTable *)stringOrderTable;
- (BOOL)isEqual:anObject;
- (int)compareTo:sender;
- (int)compareTo:sender n:(int)n;
- (int)compareTo:sender caseSensitive:(BOOL)sense;
- (int)compareTo:sender n:(int)n caseSensitive:(BOOL)sense;
- (int)cmp:(const char *)aString;
- (int)cmp:(const char *)aString n:(int)n;
- (int)casecmp:(const char *)aString;
- (int)casecmp:(const char *)aString n:(int)n;
- (const char *)strstr:(const char *)subString;

// like BASIC's left$(), right$(), and mid$(); all return a new instance.
- left:(int)count;
- right:(int)count;
- midFrom:(int)start to:(int)end;
- midFrom:(int)start length:(int)len;
- left:(int)count fromZone:(NXZone *)zone;
- right:(int)count fromZone:(NXZone *)zone;
- midFrom:(int)start to:(int)end fromZone:(NXZone *)zone;
- midFrom:(int)start length:(int)len fromZone:(NXZone *)zone;
- subStringRight:subString;
- subStringLeft:subString;

// private methods: do not use these!
- _unhookBuffer;


@end
