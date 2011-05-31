//
//  OlcbCanFrame.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 5/22/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbCanFrame.h"


@implementation OlcbCanFrame

- (OlcbCanFrame*)initFromString: (NSString*) line {
    if (line.length >= 10) {
        // expecting 
        // load header from initial part
        // From http://forums.macrumors.com/showthread.php?t=977076
        sscanf([[[line substringToIndex: 10] substringFromIndex: 2] UTF8String], "%x", &header);
        
    } else {
        // not in a packet format
        header = 0;
    }
    return self;
}

- (u_int32_t)header { return header; }

@end
