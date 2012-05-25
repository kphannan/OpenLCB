#include <stdint.h>
#include <stdio.h>
// #include <fixmath.h>
#include "float16.h"

int main(void)
{
  _float16_shape_type fl16;
  fl16.words.msw = 0xe0;
  fl16.words.lsw = 0xd1;
  float a = float16_to_float32(fl16);

  printf("original:   %0.20f\n", a);
  printf("float16:  0x%x\n", fl16.bits);
//  printf("float32:    %0.20f\n", float16_to_float32(fl16));
//  printf("fix16:    0x%x\n", float16_to_fix16(fl16));
//  printf("float32:    %0.20f\n", fix16_to_float(float16_to_fix16(fl16)));
 
  return 1;
}
