class CANMessage(object):
    '''Base class for CAN messages
    
    :param int src_alias: The alias of the node from which messages
       originate (i.e. this computer)

    Calling ``str()`` on ``CANMessage`` instances returns a string
    containing the OpenLCB message.
    '''

    def __init__(self, src_alias, body=''):
        self.src_alias = src_alias
        self.body = body

    @property
    def header(self):
        # Shift the Message Type Indicator to the left 12 bits to make
        # room, then append the source alias
        return hex((self.MTI << 12) + self.src_alias).upper()[2:]

    def __str__(self):
        return ':X{header}N{body};'.format(
            header=self.header,
            body=self.body,
        )

class VerifyNodeIDNumberSimple(CANMessage):
    MTI = 0x180a7

class VerifiedNodeIDNumber(CANMessage):
    MTI = 0x180b7

