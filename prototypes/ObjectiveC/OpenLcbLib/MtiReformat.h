//
//  MtiReformat.h
//  OpenLcbLib
//
//  Created by Bob Jacobsen on 5/31/11.
//  Copyright 2011 Bob Jacobsen. All rights reserved.
//

#include <sys/types.h>

u_int32_t CanHeaderFromMti(u_int16_t mti, u_int16_t source_alias, u_int16_t dest_alias, u_int8_t flags );

u_int16_t MtiFromCanHeader(u_int32_t header, u_int8_t byte0 );