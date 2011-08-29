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

#include "mini_cpp.h"


#ifdef _NOTHROW_NEW_
void *operator new(size_t size) throw() { return malloc(size); }
void *operator new[](size_t size) throw()
{
    return malloc(size);
}
void operator delete(void *p) throw() { free(p); }
extern "C" int __aeabi_atexit(void *object,void (*destructor)(void *),void *dso_handle) 
{ 
	object = object; // avoid warnings
	destructor = destructor;
	dso_handle = dso_handle;
    return 0; 
} 
#endif

#ifdef _MALLOC_STUB_
extern "C" void *malloc(size_t)
{ 
    return (void *)0; 
} 

extern "C" void free(void *)
{ 

}
#endif