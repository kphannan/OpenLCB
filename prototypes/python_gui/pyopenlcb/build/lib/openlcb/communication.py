import logging
import socket
import serial
import sys
import warnings

logger = logging.getLogger(__name__)


class CommunicationException(Exception):
    '''Raised if an error occurs while communicating with a node'''


class OpenLCBConnection(object):
    '''Base class for OpenLCB communication mechanisms

    Subclasses should define the following methods for communicating
    with an OpenLCB bus over a given transport:

    * :py:meth:`connect`
    * :py:meth:`send`
    * :py:meth:`receive_multi`
    * :py:meth:`close`

    Instances are also *context managers*. This facilitates opening and
    closing of connections using the ``with`` statement::

        conn = EthernetConnection(hostname)
        with conn:
            conn.send(message)

    The above example will automatically open and close the connection
    upon entering and leaving the context.
    '''

    def __enter__(self):
        self.connect()
        return self

    def __exit__(self, exc_type, exc_value, tb):
        self.close()
        if exc_type is not None:
            logger.exception('Error communicating with node')

    def connect(self):
        '''Connect to the OpenLCB bus'''

        raise NotImplementedError

    def send(self, message):
        '''Send a message to the OpenLCB bus

        :param str message: The CAN frame to send

        Any object that can be converted to a string can be passed as
        the ``message`` parameter, such as
        :py:class:`openlcb.can.CANMessage` subclasses.
        '''

        raise NotImplementedError

    def receive(self):
        '''Retreive a response from the OpenLCB bus

        :returns str: A string containing the CAN frame received from
           the bus, suitable for creating a new instance of a
           :py:class:`~openlcb.can.CANMessage` subclass

        .. deprecated:: 0.1
           This method is ambiguous; use :py:meth:`receive_one` instead.
        '''

        warnings.warn(
            '{0}.receive is deprecated. Use {0}.receive_one instead'.format(
                self.__class__.__name__),
            DeprecationWarning,
            stacklevel=2
        )
        return self.receive_one()

    def receive_one(self):
        '''Retreive a single response message from the node

        :returns str: A string containing the first message received,
           suitable for creating a new instance of a
           :py:class:`~openlcb.can.CANMessage` subclass. If no response
           was received within the timeout period, ``None`` is returned.

        .. note:: Calling this method will actually empty the buffer,
           meaning that if more than one message was received, any
           subsequent messages will be discarded. If multiple response
           messages are expected, use :py:meth:`receive_multi` instead.
        '''

        try:
            return self.receive_multi()[0]
        except IndexError:
            return None

    def receive_multi():
        '''Retreive multiple responses from the OpenLCB bus

        :returns list: A list of strings containing CAN frames,
            suitable for creating new instances of a subclass of
            :py:class:`~openlcb.can.CANMessage`. If no response was
            returned within the timeout period, an empty list (i.e.
            ``[]``) is returned.
        '''

        raise NotImplementedError

    def close(self):
        '''Close the connection to the OpenLCB bus'''

        raise NotImplementedError


class EthernetConnection(OpenLCBConnection):
    '''Class for communicating with nodes via Eth2CAN

    :param str hostname: Host name or IP address of the Eth2CAN device
    :param int port: TCP port of the Eth2CAN device
    '''

    #: Amount of time to wait for a response (in seconds)
    SOCKET_TIMEOUT = 1.0
    #: Maximum amount of data to read from a response message (in bytes)
    BUFFER_SIZE = 4096

    def __init__(self, hostname, port):
        self.hostname = hostname
        self.port = port
        self._socket = None

    def connect(self):
        '''Connect to the Eth2CAN device over TCP/IP'''

        logger.debug('Connecting to Eth2CAN at {hostname}:{port}'.format(
            hostname=self.hostname,
            port=self.port))
        self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._socket.connect((self.hostname, self.port))
        logger.debug('Socket timeout set to {0}s'.format(self.SOCKET_TIMEOUT))
        self._socket.settimeout(self.SOCKET_TIMEOUT)
        logger.debug('Connected to IP address {0} port:{1}'.format(self.hostname, self.port))

    def send(self, message):
        '''Send an OpenLCB message over TCP/IP'''

        logger.debug('Sent message: {0}'.format(message))
        self._socket.send(str(message).encode() + b'\n')

    def receive_multi(self, timeout=None):
        '''Retreive multiple responses from the OpenLCB bus'''
        
        if timeout is None:
            timeout = self.SOCKET_TIMEOUT
        
        self._socket.settimeout(timeout)
        
        response = b''
        try:
            while True:
                response += self._socket.recv(self.BUFFER_SIZE)
        except socket.timeout:
            pass
        except socket.error as e:
            raise CommunicationException(e)

        response = response.decode()
        if response:
            logger.debug('Received response: {0}'.format(response.strip()))
        else:
            logger.debug('Did not receive a response within {0} s'.format(
                         self.SOCKET_TIMEOUT))
        return response.splitlines()

    def close(self):
        '''Close the TCP/IP communication socket'''

        logger.debug('Closing connection to {hostname}:{port}'.format(
            hostname=self.hostname,
            port=self.port
        ))
        self._socket.close()


class SerialConnection(OpenLCBConnection):
    '''Class for communicating with nodes via Serial Connection

    :param str com_port: Serial port device name or port number
    :param int speed: Serial port Baud rate (defaults to 500000)
    '''

    #: Amount of time to wait for a response (in seconds)
    READ_TIMEOUT = .3

    def __init__(self, com_port, speed):
        self.com_port = com_port
        self.speed = speed
        self._ser = None

    def connect(self):
        '''Open the serial port and set RTS'''

        com_port = self.com_port
        if sys.platform == 'win32':
            # Windows numbers COM ports starting with 1, so that's what the
            # user will put in. PySerial, however, takes a 0-indexed number
            # to refer to the serial ports, so we need to shift the user's
            # input number down one.
            com_port = int(com_port) - 1
        
        logger.debug('Connecting to serial at com port {port} : {baudrate} baud'.format(
            port=self.com_port,
            baudrate=self.speed))

        # TODO: These setting should probably be exposed to the user
        try:
            self._ser = serial.Serial(
                port=com_port,
                baudrate=self.speed,
                bytesize=serial.EIGHTBITS,
                parity = serial.PARITY_EVEN,
                stopbits=serial.STOPBITS_ONE,
                timeout=self.READ_TIMEOUT,
                xonxoff=True,
            )
            self._ser.setDTR(True)
            self._ser.setRTS(True)
        except serial.SerialException as e:
            raise CommunicationException(e)
        
        logger.debug('Serial timeout set to {0}s'.format(self.READ_TIMEOUT))
        logger.info('Connected to Serial Comport {port}  @ baudrate {baudrate}'.format(
            port=self.com_port,
            baudrate=self.speed))

    def send(self, frame) :
        '''Send an OpenLCB message through the serial port'''

        logger.debug('Sent message: {0}'.format(frame))
        self._ser.write(str(frame).encode() + b'\n')

    def receive_multi(self, timeout=None):
        '''Retreive multiple responses from the OpenLCB bus'''

        if timeout is None:
           timeout = self.READ_TIMEOUT

        self._ser.timeout = timeout

        responses = [r.decode().strip() for r in self._ser.readlines()]
        if responses:
            logger.debug('Received response {0}'.format('\n'.join(responses)))
        else:
            logger.error('Did not receive a response from Node witin {0}'.format(
                         timeout))

        return responses

    def close(self):
        '''Close the serial port and terminate the OpenLCB connection'''

        logger.debug('Closing connection to serial port {port}'.format(
            port=self.com_port
        ))
        self._ser.close()
