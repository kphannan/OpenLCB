/*
    File:       ReceiveController.m

    Contains:   Manages the receive tab.

    Written by: DTS

    Copyright:  Copyright (c) 2009 Apple Inc. All Rights Reserved.

    Disclaimer: IMPORTANT: This Apple software is supplied to you by Apple Inc.
                ("Apple") in consideration of your agreement to the following
                terms, and your use, installation, modification or
                redistribution of this Apple software constitutes acceptance of
                these terms.  If you do not agree with these terms, please do
                not use, install, modify or redistribute this Apple software.

                In consideration of your agreement to abide by the following
                terms, and subject to these terms, Apple grants you a personal,
                non-exclusive license, under Apple's copyrights in this
                original Apple software (the "Apple Software"), to use,
                reproduce, modify and redistribute the Apple Software, with or
                without modifications, in source and/or binary forms; provided
                that if you redistribute the Apple Software in its entirety and
                without modifications, you must retain this notice and the
                following text and disclaimers in all such redistributions of
                the Apple Software. Neither the name, trademarks, service marks
                or logos of Apple Inc. may be used to endorse or promote
                products derived from the Apple Software without specific prior
                written permission from Apple.  Except as expressly stated in
                this notice, no other rights or licenses, express or implied,
                are granted by Apple herein, including but not limited to any
                patent rights that may be infringed by your derivative works or
                by other works in which the Apple Software may be incorporated.

                The Apple Software is provided by Apple on an "AS IS" basis. 
                APPLE MAKES NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING
                WITHOUT LIMITATION THE IMPLIED WARRANTIES OF NON-INFRINGEMENT,
                MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, REGARDING
                THE APPLE SOFTWARE OR ITS USE AND OPERATION ALONE OR IN
                COMBINATION WITH YOUR PRODUCTS.

                IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT,
                INCIDENTAL OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
                TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
                DATA, OR PROFITS; OR BUSINESS INTERRUPTION) ARISING IN ANY WAY
                OUT OF THE USE, REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION
                OF THE APPLE SOFTWARE, HOWEVER CAUSED AND WHETHER UNDER THEORY
                OF CONTRACT, TORT (INCLUDING NEGLIGENCE), STRICT LIABILITY OR
                OTHERWISE, EVEN IF APPLE HAS BEEN ADVISED OF THE POSSIBILITY OF
                SUCH DAMAGE.

*/

#import "ReceiveViewController.h"

@interface ReceiveViewController ()

// Properties that don't need to be seen by the outside world.

@property (nonatomic, readonly) BOOL              isReceiving;
@property (nonatomic, retain)   NSNetService *    netService;
@property (nonatomic, retain)   NSInputStream *   networkStream;
@property (nonatomic, copy)     NSString *        filePath;
@property (nonatomic, retain)   NSOutputStream *  fileStream;

@end

@implementation ReceiveViewController

#pragma mark * Status management

// These methods are used by the core transfer code to update the UI.

- (void)_receiveDidStart
{
    NSLog(@"receiveDidStart s");
    // Clear the current image so that we get a nice visual cue if the receive fails.
    self.imageView.image = [UIImage imageNamed:@"NoImage.png"];
    self.statusLabel.text = @"Receiving";
    [self.receiveOrCancelButton setTitle:@"Cancel" forState:UIControlStateNormal];
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
        self.imageView.image = [UIImage imageWithContentsOfFile:self.filePath];
        statusString = @"Receive succeeded";
    }
    self.statusLabel.text = statusString;
    [self.receiveOrCancelButton setTitle:@"Receive" forState:UIControlStateNormal];
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
    BOOL                success;
    
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

    self.statusLabel.text = @"Start open stream";
    
    // Open a stream to the server, finding the server via Bonjour.  Then configure 
    // the stream for async operation.

    self.netService = [[[NSNetService alloc] initWithDomain:@"local." type:@"_openlcb-hub._tcp." name:@"Test"] autorelease];
    assert(self.netService != nil);

    success = [self.netService getInputStream:&input outputStream:NULL];
    assert(success);
    
    self.networkStream = input;
    
    // -[NSNetService getInputStream:outputStream:] currently returns the stream 
    // with a reference that we have to release (something that's counter to the 
    // standard Cocoa memory management rules <rdar://problem/6868813>).
    
    [input release];
    
    self.networkStream.delegate = self;
    [self.networkStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    
    [self.networkStream open];

    // Tell the UI we're receiving.
    
    [self _receiveDidStart];

    NSLog(@"startReceive e");
}

- (void)_stopReceiveWithStatus:(NSString *)statusString
{
    NSLog(@"stopReceiveWithStatus s");
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

            [self _updateStatus:@"Receiving"];
            
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
                    NSString* line = [NSString stringWithCString:(const char *)rcvBuffer encoding:NSASCIIStringEncoding];
                    self.lastLabel.text = line;
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
            [self _stopReceiveWithStatus:@"Stream open error"];
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
    NSLog(@"receiveOrCancelAction s");
    #pragma unused(sender)
    if (self.isReceiving) {
        [self _stopReceiveWithStatus:@"Cancelled"];
    } else {
        [self _startReceive];
    }
    NSLog(@"receiveOrCancelAction e");
}

#pragma mark * View controller boilerplate

@synthesize imageView             = _imageView;
@synthesize statusLabel           = _statusLabel;
@synthesize lastLabel             = _lastLabel;
@synthesize activityIndicator     = _activityIndicator;
@synthesize receiveOrCancelButton = _receiveOrCancelButton;

- (void)viewDidLoad
{
    [super viewDidLoad];

    NSLog(@"RVC viewDidLoad s");
    
    assert(self.imageView != nil);
    assert(self.statusLabel != nil);
    assert(self.lastLabel != nil);
    assert(self.activityIndicator != nil);
    assert(self.receiveOrCancelButton != nil);
        
    self.activityIndicator.hidden = YES;
    self.statusLabel.text = @"Tap Receive to start this";
    NSLog(@"RVC viewDidLoad e");
}

- (void)viewDidUnload
{
    [super viewDidUnload];
    NSLog(@"viewDidUnload s");
    self.imageView = nil;
    self.statusLabel = nil;
    self.lastLabel = nil;
    self.activityIndicator = nil;
    self.receiveOrCancelButton = nil;
    NSLog(@"viewDidUnload e");
}

- (void)dealloc
{
    [self _stopReceiveWithStatus:@"Stopped"];

    [self->_imageView release];
    [self->_statusLabel release];
    [self->_lastLabel release];
    [self->_activityIndicator release];
    [self->_receiveOrCancelButton release];

    [super dealloc];
}

@end