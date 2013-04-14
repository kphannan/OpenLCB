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
@class OlcbInitializationCompleteMessage;
@class OlcbPCEventReportMessage;

@protocol OlcbMessageProcessor <NSObject>
@required
- (void)processDefaultMessage: (OlcbMessage*) msg;

@optional
- (void)processInitializationComplete: (OlcbInitializationCompleteMessage*) msg;
- (void)processPCEventReport: (OlcbPCEventReportMessage*) msg;

@end
