//
//  FastClockAppDelegate.h
//  FastClock
//
//  Created by Bob Jacobsen on 6/17/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import <UIKit/UIKit.h>

@class MainViewController;

@interface FastClockAppDelegate : NSObject <UIApplicationDelegate> {

}

@property (nonatomic, retain) IBOutlet UIWindow *window;

@property (nonatomic, retain) IBOutlet MainViewController *mainViewController;

@end
