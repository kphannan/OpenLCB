//
//  OlcbNodeID.m
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/1/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "OlcbNodeID.h"


@implementation OlcbNodeID

- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}

- (void)dealloc
{
    [super dealloc];
}

- (OlcbNodeID*)initFromNodeID: (OlcbNodeID*) nodeID {
    val[0] = nodeID->val[0];
    val[1] = nodeID->val[1];
    val[2] = nodeID->val[2];
    val[3] = nodeID->val[3];
    val[4] = nodeID->val[4];
    val[5] = nodeID->val[5];
    return self;
}

- (OlcbNodeID*)initFromArray: (u_int8_t*) bytes {
    val[0] = bytes[0];
    val[1] = bytes[1];
    val[2] = bytes[2];
    val[3] = bytes[3];
    val[4] = bytes[4];
    val[5] = bytes[5];
    return self;
}

- (Boolean) equals: (OlcbNodeID*) nodeID {
    if (nodeID == nil) return false;
    return ((val[5] == nodeID->val[5]) &&
            (val[4] == nodeID->val[4]) &&
            (val[3] == nodeID->val[3]) &&
            (val[2] == nodeID->val[2]) &&
            (val[1] == nodeID->val[1]) &&
            (val[0] == nodeID->val[0]) );
}
@end
