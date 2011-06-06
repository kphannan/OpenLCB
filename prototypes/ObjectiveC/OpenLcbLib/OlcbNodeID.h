//
//  OlcbNodeID.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/1/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//
//  Immutable class
//

#import <Foundation/Foundation.h>
#include <sys/types.h>

@interface OlcbNodeID : NSObject {
@private
    uint8_t val[6];
}

- (OlcbNodeID*)initFromArray: (u_int8_t*) bytes;
- (OlcbNodeID*)initFromNodeID: (OlcbNodeID*) nodeID;

- (Boolean) equals: (OlcbNodeID*) nodeID;

@end
