//
//  OlcbCanFrame.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 5/22/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import <Foundation/Foundation.h>
@class OlcbMessage;

@interface OlcbCanFrame : NSObject {
    u_int32_t   header;
}

- (OlcbCanFrame*)initFromString: (NSString*) line;
- (OlcbCanFrame*)initFromMessage: (OlcbMessage*) msg;

- (u_int32_t)header;

@end
