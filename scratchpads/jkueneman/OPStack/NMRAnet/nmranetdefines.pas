unit nmranetdefines;

{$IFDEF FPC}
interface
{$ENDIF}

const
//  MTI_OLCB_MSG                          = $08000;                            //
//  MTI_CAN                               = $00000;                            // Frame Type CAN Control Message
  MTI_CAN_CID0                              = $7000;                            // First 12 Bits of 48 bit Node ID
  MTI_CAN_CID1                              = $6000;                            // 2rd 12 Bits of 48 bit Node ID
  MTI_CAN_CID2                              = $5000;                            // 3nd 12 Bits of 48 bit Node ID
  MTI_CAN_CID3                              = $4000;                            // Last 12 Bits of 48 bit Node ID
  MTI_CAN_CID4                              = $3000;                            // non-OpenLCB Protocol
  MTI_CAN_CID5                              = $2000;                            // non-OpenLCB Protocol
  MTI_CAN_CID6                              = $1000;                            // non-OpenLCB Protocol
  MTI_CAN_CID_MASK                          = $7000;

  MTI_CAN_RID                               = $0700;                            // Reserve ID
  MTI_CAN_AMD                               = $0701;                            // Alias Map Definition
  MTI_CAN_AME                               = $0702;                            // Alias Mapping Enquiry
  MTI_CAN_AMR                               = $0703;                            // Alias Map Reset Frame

  MTI_MASK                              = $FFFF;
  MTI_FRAME_TYPE_MASK                   = $F000;
  MTI_FRAME_TYPE_GENERAL                = $9000;
  MASK_SOURCE_ALIAS                  = $00000FFF;                                // Masks out just the Source Alias Address


  MTI_ADDRESSED_MASK                 = $0008;
  MTI_SIMPLE_PROTOCOL_MASK           = $0010;
  MTI_EVENT_PRESENT_MASK             = $0002;

  MTI_INITIALIZATION_COMPLETE        = $0100;                                // Databytes = Full Node ID
  MTI_VERIFY_NODE_ID_NUMBER_DEST     = $0488;                                // Databytes = Destination Alias
  MTI_VERIFY_NODE_ID_NUMBER          = $0490;                                //
  MTI_VERIFIED_NODE_ID_NUMBER        = $0170;                                // {Optional Full Node ID}
  MTI_OPTIONAL_INTERACTION_REJECTED  = $0068;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_TERMINATE_DUE_TO_ERROR         = $00A8;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_PROTOCOL_SUPPORT_INQUIRY       = $0828;                                // Databytes = Destination Alias
  MTI_PROTOCOL_SUPPORT_REPLY         = $0668;                                // Databytes = Destination Alias, Protocol Flags
  MTI_CONSUMER_IDENTIFY              = $08F4;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFY_RANGE        = $04A4;                                // Databytes = EventID with Mask
  MTI_CONSUMER_IDENTIFIED_UNKNOWN    = $04C7;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_SET        = $04C4;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_CLEAR      = $04C5;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_RESERVED   = $04C6;                                // Databytes = EventID
  MTI_PRODUCER_IDENDIFY              = $0914;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFY_RANGE        = $0524;                                // Databytes = EventID with Mask
  MTI_PRODUCER_IDENTIFIED_UNKNOWN    = $0547;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_SET        = $0544;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_CLEAR      = $0545;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_RESERVED   = $0546;                                // Databytes = EventID
  MTI_EVENTS_IDENTIFY_DEST           = $0968;                                // Databytes = Destination Alias
  MTI_EVENTS_IDENTIFY                = $0970;                                //
  MTI_EVENT_LEARN                    = $0594;                                // Databytes = EventID
  MTI_PC_EVENT_REPORT                = $05B4;                                // Databytes = EventID  (Infamouse PCER)
  MTI_TRACTION_PROTOCOL              = $05EA;                                // Databytes = Protocol (Train Protocol, DCC, etc), Operation, control
  MTI_TRACTION_REPLY                 = $05E8;                                // Databytes = Reply data/information
  MTI_XPRESSNET                      = $0820;
  MTI_REMOTE_BUTTON_REQUEST          = $0948;
  MTI_REMOTE_BUTTON_REPLY            = $0949;
  MTI_SIMPLE_NODE_INFO_REQUEST       = $0DE8;                                // Databytes = Destination Alias
  MTI_SIMPLE_NODE_INFO_REPLY         = $0A08;                                // Databytes = Destination Alias, ACDI Data
  MTI_DATAGRAM                       = $1C48;
  MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME = $A000;
  MTI_FRAME_TYPE_DATAGRAM_FRAME_START= $B000;
  MTI_FRAME_TYPE_DATAGRAM_FRAME      = $C000;
  MTI_FRAME_TYPE_DATAGRAM_FRAME_END  = $D000;
  MTI_DATAGRAM_OK_REPLY              = $0A28;                                // Databytes = Destination Alias
  MTI_DATAGRAM_REJECTED_REPLY        = $0A48;
  MTI_FRAME_TYPE_STREAM_INIT_REQUEST = $0CC8;
  MTI_FRAME_TYPE_STREAM_INIT_REPLY   = $0868;
  MTI_FRAME_TYPE_STREAM_SEND         = $F000;
  MTI_FRAME_TYPE_STREAM_PROCEED      = $0888;
  MTI_FRAME_TYPE_STREAM_COMPLETE     = $08A8;

  // These are Negavitve Logic so "xx01" = Start, "xx10" = End were the "0" is the bit of interest
  PIP_EXTENSION_START_BIT            = $1000;                                    // Flags in the Destination word for future extension of PIP
  PIP_EXTENSION_END_BIT              = $2000;                                    // Flags in the Destination word for future extension of PIP
  PIP_EXTENSION_START_BIT_MASK       = $2000;                                    // Confusing for sure....
  PIP_EXTENSION_END_BIT_MASK         = $1000;                                    // Confusing for sure....
  PIP_EXTENSION_START_END_BIT        = $0000;                                    // Both Start and End are "set" (active zero)

implementation

end.
