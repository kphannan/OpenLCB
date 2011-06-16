//
//  OlcbMessageProcessor.h
//  OpenLcbLib
//
//  Subclass for classes that will process specific OpenLCB message types.
//
//  Created by Bob Jacobsen on 6/12/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import <Foundation/Foundation.h>
@class OlcbMessage;

@protocol OlcbMessageProcessor <NSObject>
@required
- (void)processDefaultMessage: (OlcbMessage*) msg;

@optional
- (void)processInitializationComplete: (OlcbMessage*) msg;
- (void)processPCEventReceived: (OlcbMessage*) msg;

@end
