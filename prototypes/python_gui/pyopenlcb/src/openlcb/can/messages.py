# Copyright 2012 Dustin C. Hatch, Timothy C. Hatch
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
from openlcb import can

# Here for backward compatibility's sake
parse_frame = can.parse_frame

#Base Messages
class InitializationComplete(can.GlobalMessage, can.NodeIDMessage): #
    MTI = 0x19100

class VerifiedNodeAddressed(can.AddressedMessage):
    MTI = 0x19488

class VerifyNodeIDNumberSimple(can.GlobalMessage): #
    MTI = 0x19490

class VerifiedNodeIDNumber(can.GlobalMessage, can.NodeIDMessage): #
    MTI = 0x19170

class InteractionRejected(can.AddressedMessage):
    MTI = 0x19068
    #BODY_DATA = 0x0c

#Protocol Support Messages
class PIPMessageSend(can.AddressedMessage):
    MTI = 0x19828

class PIPMessageRecv(can.AddressedMessage):
    MTI = 0x19668

#Event Exchange Messages
class IdentifyConsumers(can.GlobalMessage, can.EventMessage): #
    MTI = 0x198f4

class ConsumerIdentifyRange(can.GlobalMessage, can.EventMessage):
    MTI = 0x194a4

class ConsumerIdentifiedValid(can.GlobalMessage, can.EventMessage): #
    MTI = 0x194c4
    
class ConsumerIdentifiedInvalid(can.GlobalMessage, can.EventMessage): #
    MTI = 0x194c5

class ConsumerIdentifiedReserved(can.GlobalMessage, can.EventMessage): #
    MTI = 0x194c6

class ConsumerIdentifiedUnknown(can.GlobalMessage, can.EventMessage): #
    MTI = 0x194c7

class IdentifyProducers(can.GlobalMessage, can.EventMessage):
    MTI = 0x19914
    
class ProducerIdentifyRange(can.GlobalMessage, can.EventMessage):
    MTI = 0x19524
    
class IdentifyProducersUnknown(can.GlobalMessage, can.EventMessage):
    MTI = 0x19547

class IdentifyProducersInvalid(can.GlobalMessage, can.EventMessage):
    MTI = 0x19545

class ProducerIdentified(can.GlobalMessage, can.EventMessage): #
    MTI = 0x19544
    
class ProducerIdentifiedReserved(can.GlobalMessage, can.EventMessage):
    MTI = 0x19546

class IdentifyEventsAddressed(can.AddressedMessage):
    MTI = 0x19968

class IdentifyEventsGlobal(can.GlobalMessage): #
    MTI = 0x19970

class SendProducerConsumerEvent(can.GlobalMessage, can.EventMessage): #
    MTI = 0x195b4

#Other Messages
class SimpleNodeIdentInfo(can.GlobalMessage, can.AddressedMessage):
    MTI = 0x19de8

class SimpleNodeIdentInfoAck(can.GlobalMessage, can.AddressedMessage):
    MTI = 0x19a08

#Datagram Messages
class GeneralDatagram(can.GlobalMessage, can.DatagramMessage):#
    MTI = 0x1a

class StartDatagramFrame(can.GlobalMessage, can.DatagramMessage): #
    MTI = 0x1b

class DatagramIntermediate(can.GlobalMessage, can.DatagramMessage): #
    MTI = 0x1c

class DatagramLast(can.GlobalMessage, can.DatagramMessage): #
    MTI = 0x1d

class DatagramConfiguration(can.GlobalMessage, can.DatagramMessage): #
    MTI = 0x1a
    BODY_DATA = 0x20

class DatagramReceived(can.AddressedMessage):
    MTI = 0x19a28

class DatagramRejected(can.AddressedMessage):
    MTI = 0x19a48
    BODY_DATA = 0x4d

class DatagramTypeNotAccepted(can.GlobalMessage, can.AddressedMessage):
    MTI = 0x19a48
    BODY_DATA = 0x0AAA1040

class DatagramRejectedBufferFull(can.DatagramMessage):
    MTI = 0x1a
    BODY_DATA = 0x4d2000

class DatagramOutofOrder(can.DatagramMessage):
    MTI = 0x1a
    BODY_DATA =0x4d6000
    
class DatagramGeneral_1(can.DatagramMessage):
    MTI = 0x1b
    BODY_DATA = 0x20510000

class DatagramGeneral_2(can.DatagramMessage):
    MTI = 0x1d
    BODY_DATA = 0x5EE50040

class ResetDatagram(can.GlobalMessage, can.DatagramMessage):
    MTI = 0x1a
    BODY_DATA = 0x20A9

#CAN-specific Control Frames
class AliasMapDefinition(can.GlobalMessage):
    MTI = 0x10701

class AliasMapEquiry(can.GlobalMessage, can.NodeIDMessage):
    MTI = 0x10702

class ReserveID(can.GlobalMessage):
    MTI = 0x10700

class CidMessage1(can.GlobalMessage):
    MTI = 0x17

class CidMessage2(can.GlobalMessage):
    MTI = 0x16

class CidMessage3(can.GlobalMessage):
    MTI = 0x15

class CidMessage4(can.GlobalMessage):
    MTI = 0x14

class AliasMapRequest(can.GlobalMessage):
    MTI = 0x10703