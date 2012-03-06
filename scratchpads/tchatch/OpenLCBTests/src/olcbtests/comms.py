import logging
import socket

logger = logging.getLogger(__name__)

class CommunicationException(Exception):
    '''Raised if an error occurs while communicating with a node'''

class EthernetConnection(object):
    '''Class for communicating with nodes via Eth2CAN

    :param str hostname: Host name or IP address of the Eth2CAN device
    :param int port: TCP port of the Eth2CAN device
    '''

    #: Amount of time to wait for a response
    SOCKET_TIMEOUT = 1.0 #seconds
    #: Maximum amount of data to read from a response message
    BUFFER_SIZE = 1024 #bytes

    def __init__(self, hostname, port):
        self.hostname = hostname
        self.port = port
        self._socket = None

    def __enter__(self):
        self.connect()
        return self

    def __exit__(self, exc_type, exc_value, trace):
        if exc_type is not None:
            logger.exception('Error communicating with node')

    def connect(self):
        '''Connect to the Eth2CAN device over TCP/IP'''

        logger.debug('Connecting to Eth2CAN at {hostname}:{port}'.format(
            hostname=self.hostname,
            port=self.port))
        self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._socket.connect((self.hostname, self.port))
        logger.debug('Socket timeout set to {0}s'.format(self.SOCKET_TIMEOUT))
        self._socket.settimeout(self.SOCKET_TIMEOUT)

    def send(self, message):
        '''Send a CAN message
        
        :param CANMessage message: An instance of a
           :py:class:`~olcbtests.messages.can.CANMessage` containing
           the message to send
        '''
        
        logger.debug('Sent message: {0}'.format(message))
        self._socket.send(str(message) + '\n')

    def receive(self):
        '''Retreive a response from the node
        
        :returns str: A string containing the CAN message, suitable for
           creating a new instance of a
           :py:class:`~olcbtests.messages.can.CANMessage` subclass
        '''
        
        try:
            response = self._socket.recv(self.BUFFER_SIZE)
        except socket.error, e:
            raise CommunicationException(e)

        logger.debug('Received response: {0}'.format(response.strip()))
        return response

    def close(self):
        '''Close the TCP/IP communication socket'''

        logger.debug('Closing connection to {hostname}:{port}'.format(
            hostname=self.hostname,
            port=self.port
        ))
        self._socket.close()