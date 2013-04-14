//
//  MainViewController.h
//  Monitor
//
//  Created by Bob Jacobsen on 5/16/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "FlipsideViewController.h"
#import <UIKit/UIKit.h>

@interface MainViewController : UIViewController <FlipsideViewControllerDelegate, NSStreamDelegate> {
    UILabel *                   _statusLabel;
    UILabel *                   _lastLabel;
    UIActivityIndicatorView *   _activityIndicator;
    UIButton *                  _receiveOrCancelButton;
    
    NSNetService *              _netService;
    NSInputStream *             _networkStream;
    NSString *                  _filePath;
    NSOutputStream *            _fileStream;
    
    uint8_t                     rcvBuffer[100];
    uint8_t*                    rcvPtr;
    
    NSString *                  _hostAddress;
}

// From FlipsideView
- (IBAction)showInfo:(id)sender;

// From ReceiveViewController implementation
@property (nonatomic, retain) IBOutlet UILabel *                   statusLabel;
@property (nonatomic, retain) IBOutlet UILabel *                   lastLabel;
@property (nonatomic, retain) IBOutlet UIActivityIndicatorView *   activityIndicator;
@property (nonatomic, retain) IBOutlet UIButton *                  receiveOrCancelButton;

@property (nonatomic, copy)   NSString *                           hostAddress;

- (IBAction)receiveOrCancelAction:(id)sender;

@end
