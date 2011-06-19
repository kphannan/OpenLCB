#ifndef __OLCB_ALIASCACHE_H__
#define __OLCB_ALIASCACHE_H__

#include <stdlib.h>
#include <string.h>
#include "WProgram.h"
#include "OLCB_NodeID.h"

class OLCB_AliasCache
{
 public:
  OLCB_AliasCache() : _size(0)
  {
  }
  
  void init(uint8_t newSize)
  {
    if(_size)
    {
      //if already inited, release existing memory? TODO
    }
    _size = newSize;
    _nids = (OLCB_NodeID*)malloc(sizeof(OLCB_NodeID)*_size);
    _hits = (uint8_t*)malloc(sizeof(uint8_t)*_size);
    for(uint8_t i = 0; i < _size; ++i)
    {
      _nids[i].set(0,0,0,0,0,0);
      _nids[i].alias = 0;
      _hits[i] = 0;
    }
  }
  
  void add(OLCB_NodeID *nid)
  {
//    Serial.println("In AliasCache->add()");
    //find the least used entry, while making sure the alias isn't already cached.
    if(!nid->alias) //No alias? Nothing to cache!
    {
//      Serial.println("No alias, no cache!");
      return;
    }
//    else
//    Serial.print("Cacheing alias "); Serial.println(nid.alias,DEC);
    
    uint8_t leasthits = 255;
    uint8_t index = 0;
    for(uint8_t i=0; i < _size; ++i)
    {
      if(_nids[i] == *nid)
      {
        //already cached; return.
//        Serial.println("What do you know? Already in the cache!");
        return;
      }
      if(_hits[i] < leasthits)
      {
        leasthits = _hits[i];
        index = i;
      }
    }
//    Serial.print("Cacheing it in index "); Serial.println(index, DEC);
    memcpy(&(_nids[index]), nid, sizeof(OLCB_NodeID));
    _hits[index] = 1;
//    Serial.println("Leaving add()");
  }
  
  bool getAliasByNID(OLCB_NodeID *nid)
  {
    for(uint8_t i = 0; i < _size; ++i)
    {
      if(_nids[i] == *nid)
      {
        if(_hits[i] < 255) ++_hits[i];
        nid->alias = _nids[i].alias;
        return true;
      }
    }
    return false;
  }
  
  bool getNIDByAlias(OLCB_NodeID *nid)
  {
    for(uint8_t i = 0; i < _size; ++i)
    {
      if(_nids[i].alias == nid->alias)
      {
        if(_hits[i] < 255) ++_hits[i];
        memcpy(&(nid->val), &(_nids[i].val), 6);
        return true;
      }
    }
    return false;
  }
  
  bool removeByAlias(uint16_t alias)
  {
    for(uint8_t i=0; i < _size; ++i)
    {
      if(_nids[i].alias == alias)
      {
        _nids[i].set(0,0,0,0,0,0);
        _hits[i] = 0;
        return true;
      }
    }
    return false; //not in cache
  }
  
 private:
  uint8_t _size;
  OLCB_NodeID* _nids;
  uint8_t* _hits;
};

#endif