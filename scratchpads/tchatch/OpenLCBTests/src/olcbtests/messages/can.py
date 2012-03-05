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
    containing the OpenLCB message.
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
        header, body = message.split('N')
        header = header[2:] # Trim the :X from the beginning of the header
        body = body[:-1] # Trim the ; from the end of the body

        src_alias = int(header[-3:], 16)

        msg = cls(src_alias, body)
        return msg


class VerifyNodeIDNumberSimple(CANMessage):
    MTI = 0x180a7

class VerifiedNodeIDNumber(CANMessage):
    MTI = 0x180b7

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
