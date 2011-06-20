//
//  MainViewController.m
//  Monitor
//
//  Created by Bob Jacobsen on 5/16/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "MainViewController.h"

#import "OlcbMessage.h"
#import "OlcbCanFrame.h"
#import "OlcbMessageProcessor.h"
#import "OlcbPCEventReportMessage.h"

@interface TimeEventDisplay : NSObject < OlcbMessageProcessor > {
    NSString* result;
}
@property (nonatomic, retain) NSString *result;
@end
@implementation TimeEventDisplay
@synthesize result;
- (void)processDefaultMessage: (OlcbMessage*) msg {}
- (void)processPCEventReport: (OlcbPCEventReportMessage*) msg {
    // check for proper event
    if ( ([msg byte:0] != 0x01) || ([msg byte:1] != 0x01) || ([msg byte:2] != 0x99) 
        || ([msg byte:3] != 0x01) || ([msg byte:4] != 0x01) || ([msg byte:5] != 01) ) return;
    int hour = [msg byte:6];
    int minutes = [msg byte:7];
    self.result = [NSString stringWithFormat:@"%2d:%02d", hour, minutes];
}
@end

static TimeEventDisplay* formatter;

// Stream stand-in from iOS Development Library QA1652
// https://developer.apple.com/library/ios/#qa/qa1652/_index.html%23//apple_ref/doc/uid/DTS40008977

@interface NSStream (MyAdditions)

+ (void)getStreamsToHostNamed:(NSString *)hostName 
                         port:(NSInteger)port 
                  inputStream:(NSInputStream **)inputStreamPtr 
                 outputStream:(NSOutputStream **)outputStreamPtr;

@end

@implementation NSStream (MyAdditions)

+ (void)getStreamsToHostNamed:(NSString *)hostName 
                         port:(NSInteger)port 
                  inputStream:(NSInputStream **)inputStreamPtr 
                 outputStream:(NSOutputStream **)outputStreamPtr
{
    CFReadStreamRef     readStream;
    CFWriteStreamRef    writeStream;
    
    assert(hostName != nil);
    assert( (port > 0) && (port < 65536) );
    assert( (inputStreamPtr != NULL) || (outputStreamPtr != NULL) );
    
    readStream = NULL;
    writeStream = NULL;
    
    CFStreamCreatePairWithSocketToHost(
                                       NULL, 
                                       (CFStringRef) hostName, 
                                       port, 
                                       ((inputStreamPtr  != nil) ? &readStream : NULL),
                                       ((outputStreamPtr != nil) ? &writeStream : NULL)
                                       );
    
    if (inputStreamPtr != NULL) {
        *inputStreamPtr  = [NSMakeCollectable(readStream) autorelease];
    }
    if (outputStreamPtr != NULL) {
        *outputStreamPtr = [NSMakeCollectable(writeStream) autorelease];
    }
}

@end


@interface MainViewController ()

// Properties that don't need to be seen by the outside world.

@property (nonatomic, readonly) BOOL              isReceiving;
@property (nonatomic, retain)   NSNetService *    netService;
@property (nonatomic, retain)   NSInputStream *   networkStream;
@property (nonatomic, copy)     NSString *        filePath;
@property (nonatomic, retain)   NSOutputStream *  fileStream;

@end

@implementation MainViewController


- (void)flipsideViewControllerDidFinish:(FlipsideViewController *)controller
{
    // retrieve the host address!
    self.hostAddress = controller.hostAddress.text;
    NSLog(@"Set host address %@", self.hostAddress);
    // and drop back to front display
    [self dismissModalViewControllerAnimated:YES];
}

- (IBAction)showInfo:(id)sender
{    
    FlipsideViewController *controller = [[FlipsideViewController alloc] initWithNibName:@"FlipsideView" bundle:nil];
    controller.delegate = self;
    
    controller.modalTransitionStyle = UIModalTransitionStyleFlipHorizontal;
    [self presentModalViewController:controller animated:YES];
    
    [controller release];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations.
    return YES; // (interfaceOrientation == UIInterfaceOrientationPortrait);
}

- (void)didReceiveMemoryWarning
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc. that aren't in use.
}

// 
// Start ReceiveViewController implementation
//

#pragma mark * Status management

// These methods are used by the core transfer code to update the UI.

- (void)_receiveDidStart
{
    // Clear the current image so that we get a nice visual cue if the receive fails.
    self.statusLabel.text = @"Connecting";
    [self.receiveOrCancelButton setTitle:@"Stop" forState:UIControlStateNormal];
    [self.activityIndicator startAnimating];
    // [[AppDelegate sharedAppDelegate] didStartNetworking]; // increase connection count
}

- (void)_updateStatus:(NSString *)statusString
{
    assert(statusString != nil);
    self.statusLabel.text = statusString;
}

- (void)_receiveDidStopWithStatus:(NSString *)statusString
{
    if (statusString == nil) {
        assert(self.filePath != nil);
        statusString = @"Stop from remote end";
    }
    self.statusLabel.text = statusString;
    self.lastLabel.text = @"";  // clear time while stopped
    [self.receiveOrCancelButton setTitle:@"Start" forState:UIControlStateNormal];
    [self.activityIndicator stopAnimating];
    // [[AppDelegate sharedAppDelegate] didStopNetworking]; // decrease connection count
}

#pragma mark * Core transfer code

// This is the code that actually does the networking.

@synthesize netService    = _netService;
@synthesize networkStream = _networkStream;
@synthesize filePath      = _filePath;
@synthesize fileStream    = _fileStream;

- (BOOL)isReceiving
{
    return (self.networkStream != nil);
}

- (NSString *)pathForTemporaryFileWithPrefix:(NSString *)prefix
{
    NSString *  result;
    CFUUIDRef   uuid;
    CFStringRef uuidStr;
    
    uuid = CFUUIDCreate(NULL);
    assert(uuid != NULL);
    
    uuidStr = CFUUIDCreateString(NULL, uuid);
    assert(uuidStr != NULL);
    
    result = [NSTemporaryDirectory() stringByAppendingPathComponent:[NSString stringWithFormat:@"%@-%@", prefix, uuidStr]];
    assert(result != nil);
    
    CFRelease(uuidStr);
    CFRelease(uuid);
    
    return result;
}

- (void)_startReceive
{
    NSInputStream *     input;
        
    assert(self.networkStream == nil);      // don't tap receive twice in a row!
    assert(self.fileStream == nil);         // ditto
    assert(self.filePath == nil);           // ditto
    
    // Open a stream for the file we're going to receive into.
    
    self.filePath = [self pathForTemporaryFileWithPrefix:@"Receive"];
    assert(self.filePath != nil);
    
    self.fileStream = [NSOutputStream outputStreamToFileAtPath:self.filePath append:NO];
    assert(self.fileStream != nil);
    
    [self.fileStream open];
    
    self.statusLabel.text = @"Starting";
    
    // Open a stream to the server, finding the server via Bonjour.  Then configure 
    // the stream for async operation.
    
    self.netService = [[[NSNetService alloc] initWithDomain:@"local." type:@"_openlcb-hub._tcp." name:@"Test"] autorelease];
    
    assert(self.netService != nil);
    
    [self.netService getInputStream:&input outputStream:NULL];
    
    // -[NSNetService getInputStream:outputStream:] currently returns the stream 
    // with a reference that we have to release (something that's counter to the 
    // standard Cocoa memory management rules <rdar://problem/6868813>).
    
    [input release];
    
    //if (!success) {
    // Open a stream to the server directly.
    
    // iStream and oStream are instance variables
    //[NSStream getStreamsToHostNamed:@"10.0.1.98" port:23 inputStream:&input outputStream:nil];
    [NSStream getStreamsToHostNamed:self.hostAddress port:23 inputStream:&input outputStream:nil];
    [input retain];
    //}
    
    self.networkStream = input;
    
    self.networkStream.delegate = self;
    [self.networkStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    
    [self.networkStream open];
    
    // Tell the UI we're receiving.
    
    [self _receiveDidStart];
}

- (void)_stopReceiveWithStatus:(NSString *)statusString
{
    self.netService = nil;
    if (self.networkStream != nil) {
        self.networkStream.delegate = nil;
        [self.networkStream removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
        [self.networkStream close];
        self.networkStream = nil;
    }
    if (self.fileStream != nil) {
        [self.fileStream close];
        self.fileStream = nil;
    }
    [self _receiveDidStopWithStatus:statusString];
    self.filePath = nil;
}

- (void)stream:(NSStream *)aStream handleEvent:(NSStreamEvent)eventCode
// An NSStream delegate callback that's called when events happen on our 
// network stream.
{
#pragma unused(aStream)
    assert(aStream == self.networkStream);
    
    switch (eventCode) {
        case NSStreamEventOpenCompleted: {
            [self _updateStatus:@"Opened connection"];
            rcvPtr = rcvBuffer;
        } break;
        case NSStreamEventHasBytesAvailable: {
            NSInteger       bytesRead;
            
            [self _updateStatus:@""]; // Was "Receiving", but default normal status is nothing shown
            
            // Pull some data off the network.
            // This a brute force, one byte at a time approach for now.
            
            bytesRead = [self.networkStream read:rcvPtr maxLength:1];
            if (bytesRead == -1) {
                NSLog(@"stream read error");
                [self _stopReceiveWithStatus:@"Network read error"];
            } else if (bytesRead == 0) {
                NSLog(@"stream bytes read == 0");
                [self _stopReceiveWithStatus:nil];
            } else {
                if (*rcvPtr == 0x0d || *rcvPtr == 0x0A) {
                    // process input line from zero-terminated string
                    *rcvPtr = 0x00;
                    
                    // format and place output
                    NSString* line = [NSString stringWithCString:(const char *)rcvBuffer encoding:NSASCIIStringEncoding];
                    
                    OlcbCanFrame* frame = [[OlcbCanFrame alloc] initFromString: line];
                    OlcbMessage* msg = [frame message];
                    
                    if (formatter == nil) {
                        formatter = [[TimeEventDisplay alloc] init];
                    }
                    [msg dispatch: formatter];
                    
                    self.lastLabel.text = [formatter result];
                    
                    // and move on to next
                    rcvPtr = rcvBuffer;
                } else {
                    rcvPtr++;
                }
            }
        } break;
        case NSStreamEventHasSpaceAvailable: {
            assert(NO);     // should never happen for the output stream
        } break;
        case NSStreamEventErrorOccurred: {
            NSLog(@"stream open error");
            [self _stopReceiveWithStatus:@"Connection failed"]; // Was "Stream open error"
        } break;
        case NSStreamEventEndEncountered: {
            // ignore
        } break;
        default: {
            assert(NO);
        } break;
    }
}

#pragma mark * Actions

- (IBAction)receiveOrCancelAction:(id)sender
{
#pragma unused(sender)
    if (self.isReceiving) {
        [self _stopReceiveWithStatus:@"Tap Start to Proceed"];  // No message in normal end state
    } else {
        self.lastLabel.text = @""; // clear last message
        [self _startReceive];
    }
}

#pragma mark * View controller boilerplate

@synthesize statusLabel           = _statusLabel;
@synthesize lastLabel             = _lastLabel;
@synthesize activityIndicator     = _activityIndicator;
@synthesize receiveOrCancelButton = _receiveOrCancelButton;

@synthesize hostAddress           = _hostAddress;

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    self.hostAddress = @"10.0.1.98"; // default
    
    assert(self.statusLabel != nil);
    assert(self.lastLabel != nil);

    assert(self.activityIndicator != nil);
    assert(self.receiveOrCancelButton != nil);
    
    self.activityIndicator.hidden = YES;
    self.statusLabel.text = @"Tap Start to Proceed"; // initial content
}

- (void)viewDidUnload
{
    [super viewDidUnload];

    self.statusLabel = nil;
    self.lastLabel = nil;
    
    self.hostAddress = nil;
    
    self.activityIndicator = nil;
    self.receiveOrCancelButton = nil;

}

- (void)dealloc
{
    [self _stopReceiveWithStatus:@"Stopped"];
    
    [self->_statusLabel release];
    [self->_lastLabel release];
        
    [self->_hostAddress release];

    [self->_activityIndicator release];
    [self->_receiveOrCancelButton release];
    
    [super dealloc];
}

@end
