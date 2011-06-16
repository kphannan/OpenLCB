//
//  OlcbInitializationCompleteMessage.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 6/15/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "OlcbMessage.h"
#import "OlcbMessageProcessor.h"

@interface OlcbInitializationCompleteMessage : OlcbMessage {

}

- (void) dispatch: (id <OlcbMessageProcessor>) processor;

@end
