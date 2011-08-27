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

#ifndef __MINI_CPP_H__
#define __MINI_CPP_H__

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <reent.h>
#include <sys/stat.h>

// 'new' throws an exception if it cant find enough free RAM. This defn will provide a non-throwing version
// of new which uses malloc and saves significant code space 
// About 47kB is saved at -O2 using Sourcery g++ Lite 2010q1-188 gcc version 4.4.1
#define _NOTHROW_NEW_

// if the project is completely deterministic and you are sure that dynamic memory allocation on the heap is unnecessary
// code size can be further reduced by replacing the standard malloc() with a stub which always returns 0. 
// About 2kB is saved at -O2 using Sourcery g++ Lite 2010q1-188 gcc version 4.4.1)
//#define _MALLOC_STUB_

#endif