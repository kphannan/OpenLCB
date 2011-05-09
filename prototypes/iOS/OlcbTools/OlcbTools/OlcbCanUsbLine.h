//
//  OlcbCanUsbLine.h
//  OlcbTools
//
// Contains a single line received from a CAN USB adapter.
// 
// Immutable once created.
//
//  Created by Bob Jacobsen on 5/8/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface OlcbCanUsbLine : NSObject {
    NSString *  displayable;
    u_int32_t   header;
    u_int64_t   content;
    u_int8_t    count;
}

@end
