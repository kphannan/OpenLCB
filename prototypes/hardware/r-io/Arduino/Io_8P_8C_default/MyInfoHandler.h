#ifndef __MYINFOHANDLER_H__
#define __MYINFOHANDLER_H__

#include "OLCB_Virtual_Node.h"

/**
 * A class for generating basic canned messages for PIP and SNIP.
 */

class MyInfoHandler: public OLCB_Virtual_Node
{
  public:

    void update(void);
    bool handleMessage(OLCB_Buffer *buffer);
    void create(OLCB_Link *link, OLCB_NodeID *nid);
};

#endif
