//
//  OlcbCanFrameTests.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 5/31/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//
//  See Also: http://developer.apple.com/iphone/library/documentation/Xcode/Conceptual/iphone_development/135-Unit_Testing_Applications/unit_testing_applications.html

//  Application unit tests contain unit test code that must be injected into an application to run correctly.
//  Define USE_APPLICATION_UNIT_TEST to 0 if the unit test code is designed to be linked into an independent test executable.

#import <SenTestingKit/SenTestingKit.h>
//#import "application_headers" as required


@interface OlcbCanFrameTests : SenTestCase {
    
}

- (void)testAlloc;

/**
 * Load from a string in GridConnect format, e.g. :X182DF285N0203040506080082;
 */
- (void)testInitFromString;

@end
