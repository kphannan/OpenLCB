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

class CANMessage(object):
    '''Base class for CAN messages
    
    :param int src_alias: The alias of the node from which messages
       originate (i.e. this computer)

    Calling ``str()`` on ``CANMessage`` instances returns a string
    containing the OpenLCB frame.

    .. py:attribute:: src_alias

       The node alias of the node sending the message

    .. py:attribute:: header

       The value of the header that would be sent in the frame

    .. py:attribute:: body

       The value of the body that would be sent with the frame

    .. py:attribute:: MTI

       The message type indicator, as an integer. This value should be
       set by subclasses of :py:class:CANMessage. See
       http://www.openlcb.org/trunk/specs/MtiAllocations.pdf for a list
       of MTI allocations.
    '''

    def __init__(self, src_alias, body=''):
        self.src_alias = NodeAlias(src_alias)
        self.body = body

    @property
    def header(self):
        # Shift the Message Type Indicator to the left 12 bits to make
        # room, then append the source alias
        return hex((self.MTI << 12) + int(self.src_alias)).upper()[2:]

    def __str__(self):
        return ':X{header}N{body};'.format(
            header=self.header,
            body=self.body,
        )

    @classmethod
    def from_string(cls, message):
        '''Create a :py:class:CANMessage instance from a frame string
        
        :param str message: The complete message frame, including
           control characters
        :returns: A new instance of the :py:class:CANMessage subclass

        Subclasses of :py:class:CANMessage should override this method
        and provide their own unique logic for parsing the header and
        body into usable properties.
        '''

        header, body = message.split('N')
        header = header[2:] # Trim the :X from the beginning of the header
        body = body.strip()[:-1] # Trim the ; from the end of the body

        src_alias = int(header[-3:], 16)

        msg = cls(src_alias, body)
        return msg


class VerifyNodeIDNumberSimple(CANMessage):
    MTI = 0x180a7

class VerifiedNodeIDNumber(CANMessage):
    MTI = 0x180b7

    #: The full ID of the node sending the message
    node_id = None

    @classmethod
    def from_string(cls, message):
        msg = super(VerifiedNodeIDNumber, cls).from_string(message)
        b = msg.body
        msg.node_id = [int(b[i:i+2], 16) for i in range(0, len(b), 2)]
        return msg

class GeneralDatagram(CANMessage):
    MTI = 0x1d

    def __init__(self, src_alias, dst_alias, body):
        super(GeneralDatagram, self).__init__(src_alias, body)
        self.dst_alias = dst_alias

    @property
    def header(self):
        header = (self.MTI << 12) + int(self.dst_alias)
        header = (header << 12) + int(self.src_alias)
        return hex(header).upper()[2:]
