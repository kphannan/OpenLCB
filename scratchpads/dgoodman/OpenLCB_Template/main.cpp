/*Copyright (C) 2011 by Sagar G V

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#include "LPC17xx.h"
#include "arduino.h"

/***********LED Blinky Example**********************************************************************/
int main()
{
	LPC_GPIO0->FIODIR |= (1 << 22); // set P0.22 to output
	LPC_GPIO0->FIOPIN &= ~(1 << 22); //and turn the LED off.
	InitTimers(); //initialize the timer;
	while(1)
	{
		LPC_GPIO0->FIOPIN ^= (1 << 22); // Toggle P1.29
		delay(500);
	}
	return 0;
}



/***************************************************************************************************
Example demonstrating C++ and interrupts
*****************************************************************************************************/

/*
class testClass
{
int test;
public:
		testClass() { test = 8; }
		int getTest() { return test; }
};

int bssTest;
int dataTest = 10;
testClass myTestObj;
////////////////////////////////////////////

class ledBlinky
{
public:
		ledBlinky();
		void blinkyHandler(); 
};

ledBlinky *ledBlinkyObj;

void ledBlinky::blinkyHandler()
{
    if((LPC_TIM0->IR & 0x01) == 0x01) // if MR0 interrupt
    {
        LPC_TIM0->IR |= 1 << 0; // Clear MR0 interrupt flag
        LPC_GPIO1->FIOPIN ^= 1 << 29; // Toggle the LED
    }
}
ledBlinky::ledBlinky()
{
	ledBlinkyObj = this;
}
/////////////////////////////////////////////
int main (void)
{
	ledBlinkyObj = new ledBlinky();
	
	if(bssTest != 0) while(1); // check if global vars in bss segment are initialized to 0
	if(dataTest != 10) while(1); // check if global vars in data segment are initialized
	if(myTestObj.getTest() != 8) while(1); // check if static object constructor is called
	
	int *i = new int;	
	*i = 100;
	if(*i != 100) while(1); // test heap
	
	testClass* testObj;
	testObj = &myTestObj;
	
	if(testObj->getTest() != 8) while(1);
	
	testObj = new testClass();
	if(testObj->getTest() != 8) while(1);
	
    LPC_SC->PCONP |= 1 << 1; //Power up Timer 0
    LPC_SC->PCLKSEL0 |= 1 << 2; // Clock for timer = CCLK
    LPC_TIM0->MR0 = 1 << 23; // Give a value suitable for the LED blinking frequency based on the clock frequency
    LPC_TIM0->MCR |= 1 << 0; // Interrupt on Match0 compare
    LPC_TIM0->MCR |= 1 << 1; // Reset timer on Match 0.
    LPC_TIM0->TCR |= 1 << 1; // Manually Reset Timer0 ( forced )
    LPC_TIM0->TCR &= ~(1 << 1); // stop resetting the timer.
    NVIC_EnableIRQ(TIMER0_IRQn); // Enable timer interrupt
    LPC_TIM0->TCR |= 1 << 0; // Start timer


    LPC_SC->PCONP |= ( 1 << 15 ); // power up GPIO
    LPC_GPIO1->FIODIR |= 1 << 29; // puts P1.29 into output mode. LED is connected to P1.29
	
	
	

    while(1)
    {
        
    }
    return 0;
 
}
////////////////////////////////////////////////////////////
extern "C" void TIMER0_IRQHandler (void)
{
    ledBlinkyObj->blinkyHandler();
}

*/
