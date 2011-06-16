//
//  OlcbPrettyPrint.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/15/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "OlcbMessageProcessor.h"

@interface OlcbPrettyPrint : NSObject < OlcbMessageProcessor > {
    
}

@property (nonatomic, retain) NSString *result;

@end
