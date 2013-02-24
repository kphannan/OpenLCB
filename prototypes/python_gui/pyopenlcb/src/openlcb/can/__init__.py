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

    * Calling ``int`` on an instance of :py:class:`NodeAlias` will return
      the integer value.
    * Calling ``str`` on an instance of :py:class:`NodeAlias` will return
      a hexidecimal number.
    '''

    def __init__(self, alias):
        self._alias = int(alias)

    def __int__(self):
        return self._alias

    def __str__(self):
        return '0x{0:X}'.format(self._alias)

    def __repr__(self):
        return 'NodeAlias(0x{0:X})'.format(self._alias)

    def __eq__(self, other):
        return int(self) == other

    def __ne__(self, other):
        return int(self) != other

    def __lt__(self, other):
        return int(self) < other

    def __le__(self, other):
        return int(self) <= other

    def __gt__(self, other):
        return int(self) > other

    def __ge__(self, other):
        return int(self) >= other


class InvalidMessage(Exception):
    '''Raised when attempting to parse an improperly-formatted frame'''

    def __init__(self, frame):
        super(InvalidMessage, self).__init__(
            '{0} is not a valid CAN frame'.format(frame))


class IncorrectMTI(Exception):
    '''Raised when creating a message from a string with the wrong MTI'''

    def __init__(self, mti, cls):
        super(IncorrectMTI, self).__init__(
            '{0:X} is incorrect MTI for {1}'.format(mti, cls.__name__)
        )


class AlreadyRegistered(Exception):
    '''Raised when more than one class attempt to use the same MTI'''

    def __init__(self, mti, body_data=None):
        if body_data is not None:
            mti_str = '0x{0:X} (body: {1})'.format(mti, body_data)
        else:
            mti_str = '0x{0:X}'.format(mti)
        super(AlreadyRegistered, self).__init__(
            'MTI {0} is already registered'.format(mti_str)
        )


class CANMessage(object):
    '''Base class for Controller Area Network messages

    Calling ``str()`` on ``CANMessage`` instances returns a string
    containing the CAN frame.

    .. py:attribute:: header

       The value of the header that would be sent in the frame

    .. py:attribute:: body

       The value of the body that would be sent with the frame
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
    def parse_frame(cls, frame):
        '''Parse a string containing a CAN frame into its parts

        :returns dict: A dictionary containing the valuable message parts

        Subclasses of :py:class:`CANMessage` should override this method
        and provide their own unique logic for parsing the header and
        body into usable properties.
        '''

        try:
            header, body = frame.split('N')
        except ValueError:
            raise InvalidMessage(frame)
        if header.startswith(':X'):
            header = header[2:]
            try:
                int(header, 16)
            except ValueError:
                raise InvalidMessage(frame)
        else:
            raise InvalidMessage(frame)
        body = body.strip()
        if body.endswith(';'):
            body = body[:-1]
            if body:
                try:
                    int(body, 16)
                except ValueError:
                    raise InvalidMessage(frame)
            if hasattr(cls, 'BODY_DATA'):
                body = body[8:]
        else:
            raise InvalidMessage(frame)

        return {
            'header': header,
            'body': body
        }

    @classmethod
    def from_string(cls, frame):
        '''Create a :py:class:`CANMessage` instance from a frame string

        :param str frame: The complete CAN frame, including
           control characters
        :returns: A new instance of the :py:class:`CANMessage` subclass

        Subclasses of :py:class:`CANMessage` should *not* override this
        method, but rather :py:meth:`parse_frame` instead.
        '''

        parts = cls.parse_frame(frame)

        return cls(**parts)

    @classmethod
    def from_sequence(cls, seq):
        '''Convert a sequence (list, etc.) of strings to Message objects

        :param sequence seq: A sequence of OpenLCB frame strings
        :yields: Instances of the :py:class:`CANMessage` subclass
        '''

        for frame in seq:
            yield cls.from_string(frame)


_registered_messages = {}
class RegisteredMessage(type):
    '''Metaclass for OpenLCB message classes

    Classes using :py:class:`RegisteredMessage` as their metaclass will
    automatically have their MTIs registered, which creates a reverse
    mapping from MTI to class. This registration is required for
    :py:func:`parse_frame` to discover a message class based on the MTI
    and return an instance of it.
    '''

    def __new__(mcs, name, bases, attrs):
        cls = super(RegisteredMessage, mcs).__new__(mcs, name, bases, attrs)

        try:
            mti = attrs['MTI']
        except KeyError:
            return cls

        body_data = attrs.get('BODY_DATA', None)
        if (mti, body_data) in _registered_messages:
            raise AlreadyRegistered(mti, body_data)
        _registered_messages[(mti, body_data)] = cls

        return cls


def parse_frame(frame):
    '''Parse an OpenLCB Message from a CAN frame

    :param str frame: CAN frame as a string, including control characters
    :returns: An instance of the class registered for the MTI specified
        in the frame

    The message type lookup proceeds as follows:

    1. The MTI of the message is determined by shifting the header to
       the left by 12 bits
    2. If the message contains a body, the first byte (2 characters)
       is taken as the body MTI
    3. The list of registered message classes is searched for a class
       with the same MTI and no body MTI
    4. If no class is found, the list of registered message classes is
       searched again for the same MTI, this time including the body
       MTI
    5. If no class is found, a new MTI is calculated by bit shifting
       the MTI 12 places to the right
    6. The list of registered message classes is searched for a class
       with the recalculated MTI and the body MTI
    7. If a class is still not found, a new one is created. The new
       class is called ``UnknownMessage`` and inherits directly from
       :py:class:`OpenLCBMessage`. Its MTI is set to the original MTI
       (i.e. before the second bit shift), and its body MTI is set, if
       it exists.

    Once a suitable message class has been found (or created), it will
    be instantiated by calling its ``from_string`` classmethod, passing
    in the original message frame string.
    '''

    try:
        header, body = frame.split('N')
    except (AttributeError, ValueError):
        raise InvalidMessage(frame)

    try:
        header = int(header.strip()[2:], 16)
    except ValueError:
        raise InvalidMessage(frame)

    mti = header >> 12
    try:
        body_data = int(body.strip()[:8], 16)
    except ValueError:
        body_data = None

    options = [(mti, None)]
    if body_data is not None:
        options.append((mti, body_data))
    options.append((mti >> 12, body_data))

    for h, b in options:
        try:
            cls = _registered_messages[(h, b)]
            break
        except KeyError:
            continue
    else:
        cls = type('UnknownMessage', (OpenLCBMessage,), {})
        cls.MTI = mti
        # The OpenLCB specification does not indicate when an MTI byte
        # is allowed or required in the CAN body. As such, if a message
        # class is not defined, we really have no way of knowing whether
        # or not the first byte in the body is an MTI or useful data.
        if body_data is not None:
            cls.BODY_DATA = body_data

    return cls.from_string(frame)


class OpenLCBMessage(CANMessage):
    '''Base class for all OpenLCB CAN messages

    :param int src_alias: The alias of the node from which messages
       originate

    .. py:attribute:: src_alias

       The node alias of the node sending the message

    .. py:attribute:: MTI

       The message type indicator, as an integer. This value should be
       set by subclasses of :py:class:`CANMessage`. See
       http://www.openlcb.org/trunk/specs/MtiAllocations.pdf for a list
       of MTI allocations.
    '''

    __metaclass__ = RegisteredMessage

    def __init__(self, **keywords):
        try:
            self.src_alias = NodeAlias(keywords['src_alias'])
        except KeyError:
            self.src_alias = None
        self._body = keywords.get('body', '')

    @property
    def header(self):
        # Shift the Message Type Indicator to the left 12 bits to make
        # room, then append the source alias
        return '{0:X}'.format((self.MTI << 12 | int(self.src_alias)))


    @property
    def body(self):

       if hasattr(self, 'dst_alias'):
            return '{dst_alias:>04X}{body}'.format(
                dst_alias=int(self.dst_alias),
                body=self._body
            )
       else:
            return self._body

    @classmethod
    def parse_frame(cls, frame):
        parts = super(OpenLCBMessage, cls).parse_frame(frame)
        parts['src_alias'] = int(parts['header'], 16) & 0xfff
        if not parts['src_alias']:
            raise InvalidMessage(frame)
        parts['mti'] = int(parts['header'], 16) >> 12
        return parts

    #@classmethod
    #def from_string(cls, frame):
        #parts = cls.parse_frame(frame)
        #if parts['mti'] != cls.MTI:
            #raise IncorrectMTI(parts['mti'], cls)
        #else:
            #return cls(**parts)


class GlobalMessage(OpenLCBMessage):
    '''Represents a message sent to the entire bus'''

class DatagramMessage(OpenLCBMessage):
    def __init__(self, **keywords):
        super(DatagramMessage, self).__init__(**keywords)
        try:
            self.dst_alias = NodeAlias(keywords['dst_alias'])
        except KeyError:
            self.dst_alias = None
    @property
    def body(self):

       if hasattr(self, 'BODY_DATA'):
            return '{mti:>04X}{body}'.format(
                mti=self.BODY_DATA,
                body=self._body
            )
       else:
            return self._body

    @property
    def header(self):
        header = self.MTI << 12
        header |= int(self.dst_alias)
        header <<= 12
        header |= int(self.src_alias)
        return '{0:X}'.format(header)


    @classmethod
    def parse_frame(cls, frame):
        parts = super(DatagramMessage, cls).parse_frame(frame)
        parts['mti'] >>= 12
        parts['dst_alias'] = (int(parts['header'], 16) & 0xfff000) >> 12
        return parts

    @classmethod
    def from_string(cls, frame):
        parts = cls.parse_frame(frame)
        if parts['mti'] != cls.MTI:
            raise IncorrectMTI(parts['mti'], cls)
        else:
            return cls(**parts)


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

    def __init__(self, **keywords):
        super(AddressedMessage, self).__init__(**keywords)
        try:
            self.dst_alias = NodeAlias(keywords['dst_alias'])
        except KeyError:
            self.dst_alias = None

    @property
    def header(self):
        header = self.MTI
        header <<= 12
        header |= int(self.src_alias)
        return '{0:X}'.format(header)


    @classmethod
    def parse_frame(cls, frame):
        parts = super(AddressedMessage, cls).parse_frame(frame)
        parts['mti'] >>= 12
        parts['dst_alias'] = (int(parts['header'], 16) & 0xfff000) >> 12
        return parts

    #@classmethod
    #def from_string(cls, frame):
        #parts = cls.parse_frame(frame)
        #if parts['mti'] != cls.MTI:
            #raise IncorrectMTI(parts['mti'], cls)
        #else:
            #return cls(**parts)


class NodeIDMessage(OpenLCBMessage):
    '''Represents a message that contains a full node ID

    .. py:attribute:: node_id

       The full of the ID of the node identified in the message
    '''

    def __init__(self, **keywords):
        super(NodeIDMessage, self).__init__(**keywords)
        self.node_id = keywords.get('node_id', None)

    @property
    def body(self):
        return '{mti}{node_id}'.format(
            mti=super(NodeIDMessage, self).body,
            node_id=''.join('{0:>02X}'.format(i) for i in self.node_id)
        )

    @classmethod
    def parse_frame(cls, frame):
        parts = super(NodeIDMessage, cls).parse_frame(frame)
        parts['node_id'] = [
            int(parts['body'][i:i+2], 16)
            for i in range(0, 12, 2)
        ]
        return parts


class EventMessage(OpenLCBMessage):
    '''Represents a message containing an event ID

    .. py:attribute:: event_id

       The ID of the event identified in the message

    According to the `Event Identifiers Technical Note
    <http://www.openlcb.org/trunk/specs/drafts/GenEventIdTN.pdf>`_, a
    node ID is part of the event ID, however it does not have to have
    any relationship to the event (i.e. it does not have to refer to
    the node sending the message).
    '''

    def __init__(self, **keywords):
        super(EventMessage, self).__init__(**keywords)
        self.event_id = keywords.get('event_id', None)

    @property
    def body(self):
        return '{mti}{event_id}'.format(
            mti=super(EventMessage, self).body,
            event_id=''.join('{0:>02X}'.format(i) for i in self.event_id)
        )

    @classmethod
    def parse_frame(cls, frame):
        parts = super(EventMessage, cls).parse_frame(frame)
        parts['event_id'] = [
            int(parts['body'][i:i+2], 16)
            for i in range(0, 16, 2)
        ]
        return parts

class DatagramData(OpenLCBMessage):
    '''Represents a message that contains a full node ID

    .. py:attribute:: node_id

       The full of the ID of the node identified in the message
    '''

    def __init__(self, **keywords):
        super(DatagramData, self).__init__(**keywords)
        self.data = keywords.get('data', None)

    @property
    def body(self):
        return '{mti}{node_id}'.format(
            mti=super(DatagramData, self).body,
            node_id=''.join('{0:>02X}'.format(i) for i in self.data)
        )

    @classmethod
    def parse_frame(cls, frame):
        parts = super(DatagramData, cls).parse_frame(frame)
        parts['data'] = [
            int(parts['body'], 16)
            for i in range(0, 12, 2)
        ]
        return parts
