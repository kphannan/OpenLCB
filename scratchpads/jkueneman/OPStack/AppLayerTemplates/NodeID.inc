// This is the block of ID's reserved by Mustangpeak, see the OpenLCB website to allocate your own IDs
//  Every OLCB Node must have a UNIQUE node ID so do not use these IDs
const
  NodeID_HI = $00050101;
// NOTE, any Virtual Nodes will be create by adding 1 to this number for each node, this means that
// a new PWB programming must accout for the virutal node ID that will be create for each PWB.
//
// Note that this number + the number of Virtual Node MUST not roll past $00FFFFFF

NodeID_LO = $00010700;
