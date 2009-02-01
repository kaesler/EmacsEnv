//
//	String.m -- a generic class to simplify manipulation of (char *)'s
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

#import "String.h"
#import <strings.h>
#import <appkit/appkit.h>

@implementation String

- init
{
	 [super init];
	 [self setStringOrderTable:NXDefaultStringOrderTable()];
	 buffer = NULL;
	 length = 0;
	 _length = 0;
	 return self;
}

- initString:(const char *)aString
{
	[self init];
	return [self setString:aString];
}

- setStringOrderTable:(NXStringOrderTable *)table
{
	orderTable = table;
	return self;
}

- (NXStringOrderTable *)stringOrderTable
{
	return orderTable;
}

- allocateBuffer:(int)size
{
	return [self allocateBuffer:size fromZone:[self zone]];
}

- allocateBuffer:(int)size fromZone:(NXZone *)zone
{
	if (size <= _length) return self;
	[self freeString];
	buffer = (char *)NXZoneMalloc(zone, size);
	buffer[0] = 0;
	_length = size;

	return self;
}

- setString:(const char *)aString
{
	return [self setString:aString fromZone:[self zone]];
}

- setString:(const char *)aString fromZone:(NXZone *)zone
{
	if (!aString) return self;
	// Note that I could have used NXCopyStringBufferFromZone() here
	// instead.  This works just as well, but it may be slower...
	// I haven't checked that out, though.  It might be worth doing.
	[self allocateBuffer:strlen(aString)+1 fromZone:zone];
	strcpy(buffer, aString);
	length = strlen(buffer);
	return self;
}

- setStringValue:sender
{
	if (![sender respondsTo:@selector(stringValue)]) return self;
	return [self setString:[sender stringValue] fromZone:[self zone]];
}

- setStringValue:sender fromZone:(NXZone *)zone
{
	if (![sender respondsTo:@selector(stringValue)]) return self;
	return [self setString:[sender stringValue] fromZone:zone];
}

- (const char *)stringValue
{
	return buffer;
}

- read:(NXTypedStream *)stream
{
	char *tmpStr;
	[super read:stream];
	NXReadType(stream, "i", &_length);
	tmpStr = (char *)malloc(_length + 1);
	NXReadType(stream, "*", &tmpStr);
	[self setString:tmpStr fromZone:[self zone]];
	free(tmpStr);
	return self;
}

- write:(NXTypedStream *)stream
{
	[super write:stream];
	NXWriteTypes(stream, "i*", &_length, &buffer);
	return self;
}

- copyFromZone:(NXZone *)zone
{
	String *myCopy = [super copyFromZone:zone];
	// force child to have it's own copy of the string buffer
	[myCopy _unhookBuffer];
	[myCopy allocateBuffer:_length fromZone:zone];
	[myCopy setString:buffer fromZone:zone];
	return myCopy;
}

- _unhookBuffer
{ // used by the copy method so that we don't free the buffer from orig. 
	buffer = NULL; _length = 0;
	return self;
}

- freeString
{
	if(buffer) free(buffer);
	buffer = NULL;
	length = 0;
	_length = 0;
	return self;
}

- free
{
	 [self freeString];
	 return [super free];
}

- cat:(const char *)aString
{
	return [self cat:aString
				 n:strlen(aString)
				 fromZone:[self zone]];
}

- cat:(const char *)aString n:(int)n
{
	return [self cat:aString n:n fromZone:[self zone]];
}

- cat:(const char *)aString fromZone:(NXZone *)zone
{
	return [self cat:aString n:strlen(aString) fromZone:zone];
}

- cat:(const char *)aString n:(int)n fromZone:(NXZone *)zone
{
	char *newBuffer; int newSize;
	newSize = length + n + 1;
	if (newSize > _length) {
		newBuffer = (char *)NXZoneMalloc(zone, newSize);
		_length = newSize;
		newBuffer[0] = '\0';
		strcat(newBuffer, buffer);
		strncat(newBuffer, aString, n);
		free(buffer);
		buffer = newBuffer;
	} else  strncat(buffer, aString, n);
	length = strlen(buffer);
	return self;
}

- concatenate:sender
{
	if (![sender respondsTo:@selector(stringValue)]) return self;
	return [self cat:[sender stringValue]
				 n:strlen([sender stringValue])
				 fromZone:[self zone]];
}

- concatenate:sender n:(int)n
{
	if (![sender respondsTo:@selector(stringValue)]) return self;
	return [self cat:[sender stringValue] n:n fromZone:[self zone]];
}

- concatenate:sender fromZone:(NXZone *)zone
{
	if (![sender respondsTo:@selector(stringValue)]) return self;
	return [self cat:[sender stringValue]
			n:strlen([sender stringValue]) fromZone:zone];
}

- concatenate:sender n:(int)n fromZone:(NXZone *)zone
{
	if (![sender respondsTo:@selector(stringValue)]) return self;
	return [self cat:[sender stringValue] n:n fromZone:zone];
}

- (const char *)rindex:(char)aChar
{
	return rindex(buffer, aChar);
}

- (const char *)index:(char)aChar
{
	return index(buffer, aChar);
}

- (const char *)strstr:(const char *)subString
{
	return strstr(buffer, subString);
}

- subStringRight:subString
{
	const char *sub;
	if ([subString respondsTo:@selector(stringValue)])
		sub = [subString stringValue];
	else return nil;	// error if can't get string value
	return [[String allocFromZone:[self zone]]
			initString:strstr(buffer, sub)];
}

- subStringLeft:subString
{
	const char *sub;
	char *tempString = NXCopyStringBufferFromZone(buffer, [self zone]);
	char *temp2;
	id retVal = [String alloc];
	
	if ([subString respondsTo:@selector(stringValue)])
		sub = [subString stringValue];
	else return nil;	// error if can't get string value
	temp2 = strstr(tempString, sub); 
	if (temp2) {
		temp2[0] = '\0';	// terminate it early
		[retVal initString:tempString];
	} else { // substring not found
		return [self copy];
	}
	free(tempString);
	return retVal;
}

- (int)length
{
	return length;
}

- (BOOL)isEqual:anObject
{
	if (anObject == self) return YES;
	// doesn't have to be a String object to be equal...
	if ([anObject respondsTo:@selector(stringValue)]) {
		if (!NXOrderStrings(buffer, [anObject stringValue],
				YES, -1, orderTable)) return YES;
	}
	return NO;
}

- (int)compareTo:sender
{
	return [self compareTo:sender n:(-1) caseSensitive:YES];
}

- (int)compareTo:sender n:(int)n
{
	return [self compareTo:sender n:n caseSensitive:YES];
}

- (int)cmp:(const char *)aString
{
	return strcmp(buffer, aString);
}

- (int)cmp:(const char *)aString n:(int)n
{
	return strncmp(buffer, aString, n);
}

- (int)compareTo:sender caseSensitive:(BOOL)sense
{
	return [self compareTo:sender n:(-1) caseSensitive:sense];
}

- (int)compareTo:sender n:(int)n caseSensitive:(BOOL)sense
{
	if (![sender respondsTo:@selector(stringValue)]) return 1; // !=
	return NXOrderStrings(buffer, [sender stringValue], sense, n, orderTable);
}

- (int)casecmp:(const char *)aString
{
	return strcasecmp(buffer, aString);
}

- (int)casecmp:(const char *)aString n:(int)n
{
	return strncasecmp(buffer, aString, n);
}

- left:(int)count
{
	return [self left:count fromZone:[self zone]];
}

- right:(int)count
{
	return [self right:count fromZone:[self zone]];
}

- midFrom:(int)start to:(int)end
{
	return [self midFrom:start to:end fromZone:[self zone]];
}

- midFrom:(int)start length:(int)len
{
	return [self midFrom:start length:len fromZone:[self zone]];
}

- left:(int)count fromZone:(NXZone *)zone
{
	char smash = buffer[count];
	id newString = [[String allocFromZone:zone] init];
	buffer[count] = '\0';
	[newString setString:buffer fromZone:zone];
	buffer[count] = smash;
	return newString;
}

- right:(int)count fromZone:(NXZone *)zone
{
	id newString = [[String allocFromZone:zone] init];
	[newString setString:&buffer[length - count] fromZone:zone];
	return newString;
}

- midFrom:(int)start to:(int)end fromZone:(NXZone *)zone
{
	char smash = buffer[end];
	id newString = [[String allocFromZone:zone] init];
	buffer[end] = '\0'; // inclusive; end-1 is not.
	[newString setString:&buffer[start - 1] fromZone:zone];
	buffer[end] = smash;
	return newString;
}

- midFrom:(int)start length:(int)len fromZone:(NXZone *)zone
{
	register int spot = start + len - 1;
	char smash = buffer[spot];
	id newString = [[String allocFromZone:zone] init];
	buffer[spot] = '\0';
	[newString setString:&buffer[start - 1] fromZone:zone];
	buffer[spot] = smash;
	return newString;
}


@end
