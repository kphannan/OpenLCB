//
//  MainViewController.m
//  Monitor
//
//  Created by Bob Jacobsen on 5/16/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#import "MainViewController.h"

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
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
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
    NSLog(@"receiveDidStart s");
    // Clear the current image so that we get a nice visual cue if the receive fails.
    self.statusLabel.text = @"Connecting";
    [self.receiveOrCancelButton setTitle:@"Stop" forState:UIControlStateNormal];
    [self.activityIndicator startAnimating];
    // [[AppDelegate sharedAppDelegate] didStartNetworking]; // increase connection count
    NSLog(@"receiveDidStart e");
}

- (void)_updateStatus:(NSString *)statusString
{
    assert(statusString != nil);
    self.statusLabel.text = statusString;
}

- (void)_receiveDidStopWithStatus:(NSString *)statusString
{
    NSLog(@"receiveDidStopWithStatus s");
    if (statusString == nil) {
        assert(self.filePath != nil);
        statusString = @"Stop from remote end";
    }
    self.statusLabel.text = statusString;
    [self.receiveOrCancelButton setTitle:@"Start" forState:UIControlStateNormal];
    [self.activityIndicator stopAnimating];
    // [[AppDelegate sharedAppDelegate] didStopNetworking]; // decrease connection count
    NSLog(@"receiveDidStopWithStatus e");
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
    
    NSLog(@"startReceive s");
    
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
    NSLog(@"Use host address %@", self.hostAddress);
    [NSStream getStreamsToHostNamed:self.hostAddress port:23 inputStream:&input outputStream:nil];
    [input retain];
    //}
    
    self.networkStream = input;
    
    self.networkStream.delegate = self;
    [self.networkStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    
    [self.networkStream open];
    
    // Tell the UI we're receiving.
    
    [self _receiveDidStart];
    
    NSLog(@"startReceive e");
}

- (void)_stopReceiveWithStatus:(NSString *)statusString
{
    NSLog(@"stopReceiveWithStatus s %@", statusString);
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
    NSLog(@"stopReceiveWithStatus e");
}

- (void)stream:(NSStream *)aStream handleEvent:(NSStreamEvent)eventCode
// An NSStream delegate callback that's called when events happen on our 
// network stream.
{
#pragma unused(aStream)
    assert(aStream == self.networkStream);
    
    switch (eventCode) {
        case NSStreamEventOpenCompleted: {
            NSLog(@"stream open connection");
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
                    self.lastLabel.text = line;
                    
                    int header = 0;
                    if (line.length >= 10) {
                        // From http://forums.macrumors.com/showthread.php?t=977076
                        sscanf([[[line substringToIndex: 10] substringFromIndex: 2] UTF8String], "%x", &header);
                        //NSInteger n = header;
                        //NSLog(@"d value %d", n);
                        //NSLog(@"x value %x", n);
                    
                        if ( header & 0x08000000 ) {
                            // OpenLCB form
                            self.typeValue.text = @"OpenLCB";
     
                            self.srcALabel.text = @"Source:";
                            if (line.length >= 10) {
                                self.srcAValue.text = [[line substringToIndex: 10] substringFromIndex: 7];
                            } else {
                                self.srcAValue.text = @"";
                            }
                            self.dstALabel.hidden = NO;
                            if (line.length >= 7) {
                                self.dstAValue.text = [[line substringToIndex: 7] substringFromIndex: 4];
                            } else {
                                self.dstAValue.text = @"";
                            }
                            
                        } else {
                            // CAN form
                            self.typeValue.text = @"CAN";
                            self.srcALabel.text = @"Number:";
                            self.srcAValue.text = [[line substringToIndex: 10] substringFromIndex: 7];
                            self.dstALabel.hidden = YES;
                            self.dstAValue.text = @"";
                        }
                    } else {
                        // not in a packet format
                        self.typeValue.text = @"";
                        self.srcALabel.text = @"";
                        self.srcAValue.text = @"";
                        self.dstALabel.hidden = YES;
                        self.dstAValue.text = @"";
                    }
                   
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
    NSLog(@"receiveOrCancelAction s");
    if (self.isReceiving) {
        [self _stopReceiveWithStatus:@""];  // No message in normal end state
    } else {
        self.lastLabel.text = @""; // clear last message
        [self _startReceive];
    }
    NSLog(@"receiveOrCancelAction e");
}

#pragma mark * View controller boilerplate

@synthesize statusLabel           = _statusLabel;
@synthesize lastLabel             = _lastLabel;
@synthesize activityIndicator     = _activityIndicator;
@synthesize receiveOrCancelButton = _receiveOrCancelButton;

@synthesize srcAValue           = _srcAValue;
@synthesize srcALabel           = _srcALabel;
@synthesize dstAValue           = _dstAValue;
@synthesize dstALabel           = _dstALabel;
@synthesize typeValue           = _typeValue;
@synthesize typeLabel           = _typeLabel;

@synthesize hostAddress           = _hostAddress;

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    NSLog(@"RVC viewDidLoad s");
    
    self.hostAddress = @"10.0.1.98"; // default
    
    assert(self.statusLabel != nil);
    assert(self.lastLabel != nil);

    assert(self.srcAValue != nil);
    assert(self.srcALabel != nil);
    assert(self.dstAValue != nil);
    assert(self.dstALabel != nil);
    assert(self.typeValue != nil);
    assert(self.typeLabel != nil);

    assert(self.activityIndicator != nil);
    assert(self.receiveOrCancelButton != nil);
    
    self.activityIndicator.hidden = YES;
    self.statusLabel.text = @""; // Was "Tap Start to begin", initial content
    NSLog(@"RVC viewDidLoad e");
}

- (void)viewDidUnload
{
    [super viewDidUnload];
    NSLog(@"viewDidUnload s");
    self.statusLabel = nil;
    self.lastLabel = nil;
    
    self.srcAValue = nil;
    self.srcALabel = nil;
    self.dstAValue = nil;
    self.dstALabel = nil;
    self.typeValue = nil;
    self.typeLabel = nil;

    self.hostAddress = nil;
    
    self.activityIndicator = nil;
    self.receiveOrCancelButton = nil;
    NSLog(@"viewDidUnload e");
}

- (void)dealloc
{
    [self _stopReceiveWithStatus:@"Stopped"];
    
    [self->_statusLabel release];
    [self->_lastLabel release];
    
    [self->_srcAValue release];
    [self->_srcALabel release];
    [self->_dstAValue release];
    [self->_dstALabel release];
    [self->_typeValue release];
    [self->_typeLabel release];
    
    [self->_hostAddress release];

    [self->_activityIndicator release];
    [self->_receiveOrCancelButton release];
    
    [super dealloc];
}

@end
