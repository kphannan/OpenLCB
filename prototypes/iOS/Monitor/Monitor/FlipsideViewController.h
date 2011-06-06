//
//  FlipsideViewController.h
//  Monitor
//
//  Created by Bob Jacobsen on 5/16/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import <UIKit/UIKit.h>

@protocol FlipsideViewControllerDelegate;

@interface FlipsideViewController : UIViewController {

    UITextField *                   _hostAddress;

}

@property (nonatomic, retain) IBOutlet UITextField *                   hostAddress;

@property (nonatomic, assign) id <FlipsideViewControllerDelegate> delegate;

- (IBAction)done:(id)sender;

@end


@protocol FlipsideViewControllerDelegate
- (void)flipsideViewControllerDidFinish:(FlipsideViewController *)controller;
@end
