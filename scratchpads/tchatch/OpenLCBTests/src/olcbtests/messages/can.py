'''OpenLCB over CAN

Classes for creating, manipulating, and parsing CAN frames as OpenLCB
messages.

:author: Dustin C. Hatch
:author: Timothy C. Hatch
'''
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

class NodeAlias(object):
    '''Utility class for handling node alias

    * Calling ``int`` on an instance of :py:class:NodeAlias will return
       the integer value.
    * Calling ``str`` on an instance of :py:class:NodeAlias will return
      a hexidecimal number.
    '''

    def __init__(self, alias):
        self._alias = int(alias)

    def __int__(self):
        return self._alias

    def __str__(self):
        return '0x{0}'.format(hex(self._alias)[2:].upper())


class InvalidMessage(Exception):
    '''Raised when attempting to parse an improperly-formatted frame'''

    def __init__(self, frame):
        super(InvalidMessage, self).__init__(
            '{0} is not a valid CAN frame'.format(frame))


class CANMessage(object):
    '''Base class for Controller Area Network messages

    Calling ``str()`` on ``CANMessage`` instances returns a string
    containing the CAN frame.

    .. py:attribute:: header

       The value of the header that would be sent in the frame

    .. py:attribute:: body

       The value of the body that would be sent with the frame

    .. py:attribute:: MTI

       The message type indicator, as an integer. This value should be
       set by subclasses of :py:class:`CANMessage`. See
       http://www.openlcb.org/trunk/specs/MtiAllocations.pdf for a list
       of MTI allocations.
    '''

    def __init__(self, header='', body=''):
        self.header = header
        self.body = body

    def __str__(self):
        return ':X{header}N{body};'.format(
            header=self.header,
            body=self.body,
        )

    @classmethod
    def _parse_frame(cls, frame):
        '''Parse a string containing a CAN frame into its parts

        :returns tuple: A two-tuple containing the frame header and
           body, in that order
        '''

        try:
            header, body = frame.split('N')
        except ValueError:
            raise InvalidMessage(frame)
        if header.startswith(':X'):
            header = header[2:]
        else:
            raise InvalidMessage(frame)
        body = body.strip()
        if body.endswith(';'):
            body = body[:-1]
        else:
            raise InvalidMessage(frame)

        return header, body

    @classmethod
    def from_string(cls, frame):
        '''Create a :py:class:`CANMessage` instance from a frame string

        :param str frame: The complete CAN frame, including
           control characters
        :returns: A new instance of the :py:class:`CANMessage` subclass

        Subclasses of :py:class:`CANMessage` should override this method
        and provide their own unique logic for parsing the header and
        body into usable properties.
        '''

        header, body = cls._parse_frame(frame)

        return cls(header, body)

    @classmethod
    def from_sequence(cls, seq):
        '''Convert a sequence (list, etc.) of strings to Message objects
        
        :param sequence seq: A sequence of OpenLCB frame strings
        :yields: The :py:class:`CANMessage` subclass
        '''

        for frame in seq:
            yield cls.from_string(frame)


class OpenLCBMessage(CANMessage):
    '''Base class for all OpenLCB CAN messages

    :param int src_alias: The alias of the node from which messages
       originate

    .. py:attribute:: src_alias

       The node alias of the node sending the message
    '''

    def __init__(self, src_alias, body=''):
        self.src_alias = NodeAlias(src_alias)
        self.body = body

    @property
    def header(self):
        # Shift the Message Type Indicator to the left 12 bits to make
        # room, then append the source alias
        return hex((self.MTI << 12) + int(self.src_alias)).upper()[2:]

    @classmethod
    def from_string(cls, frame):
       header, body = cls._parse_frame(frame)
       src_alias = int(header, 16) & 0xfff
       return cls(src_alias, body)


class GlobalMessage(OpenLCBMessage):
    '''Represents a message sent to the entire bus'''


class AddressedMessage(OpenLCBMessage):
    '''Represents a message sent to a single node

    :param int src_alias: The alias of the node from which messages
       originate
    :param int dst_alias: The alias of the node to which the message
       is sent

    .. py:attribute:: src_alias

       The node alias of the node sending the message

    .. py:attribute:: dst_alias

       The alias of the node for which the message is intended
    '''

    def __init__(self, src_alias, dst_alias, body=''):
        super(AddressedMessage, self).__init__(src_alias, body)
        self.dst_alias = NodeAlias(dst_alias)

    @property
    def header(self):
        header = self.MTI << 12
        header |= int(self.dst_alias)
        header <<= 12
        header |= int(self.src_alias)
        return hex(header).upper()[2:]


class VerifyNodeIDNumberSimple(GlobalMessage):
    MTI = 0x180a7


class MessageWithBody(OpenLCBMessage):
    '''Represents a message that contains a body'''
    
    #: The full ID of the node sending the message
    node_id = None

    @classmethod
    def from_string(cls, frame):
        msg = super(MessageWithBody, cls).from_string(frame)
        b = msg.body
        msg.node_id = [int(b[i:i+2], 16) for i in range(0, 12, 2)]
        return msg

    
class VerifiedNodeIDNumber(GlobalMessage, MessageWithBody):
    MTI = 0x180b7


class IdentifyEventsAddressed(AddressedMessage):
    MTI = 0x1e

    def __init__(self, src_alias, dst_alias, body):
        super(IdentifyEventsAddressed, self).__init__(src_alias, body)
        self.dst_alias = dst_alias


class IdentifyEventsGlobal(GlobalMessage):
    MTI = 0x182b7


class IdentifyConsumers(GlobalMessage):
    MTI = 0x1824f


class GeneralDatagram(AddressedMessage):
    MTI = 0x1d


class ConsumerIdentified(GlobalMessage, MessageWithBody):
    MTI = 0x1926b

    #: The ID of an event produced or consumed by the node
    event_id = None

    @classmethod
    def from_string(cls, frame):
        msg = super(ConsumerIdentified, cls).from_string(frame)
        msg.event_id = int(msg.body, 16) & 0xffff
        return msg
