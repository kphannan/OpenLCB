//
//  FirstViewController.m
//  OlcbTools
//
//  Created by Bob Jacobsen on 5/8/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "MonitorViewController.h"


@implementation MonitorViewController


// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad
{
    NSLog(@"MVC viewDidLoad s");
    [super viewDidLoad];
    NSLog(@"MVC viewDidLoad e");
}


- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}


- (void)didReceiveMemoryWarning
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc. that aren't in use.
}


- (void)viewDidUnload
{
    [super viewDidUnload];

    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}


- (void)dealloc
{
    [super dealloc];
}

@end