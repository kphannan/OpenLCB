'''
Send Unknown Datagram Type message

@author: Tim Hatch
'''
import openlcb.can
from openlcb.can import messages
from olcbtests.util import nodetest
import logging


logger = logging.getLogger(__name__)

def datagram_reply(src_alias, dst_alias, conn):
    msg = messages.DatagramReceived(
        src_alias=src_alias,
        dst_alias=dst_alias
        )
    #logger.info('Sending datagram reply from {src} to {dst}'.format(
        #src=msg.src_alias,
        #dst=msg.dst_alias,
    #))
    conn.send(msg)

def send_datagram_message(src_alias, dst_alias, data,  conn):
    msg = messages.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        #body='2043000000000a'.format(data)
        body='20430000004010'
        )
    #logger.debug('Sending Configuration Datagram message from {src} to {dst}'.format(
        #src=msg.src_alias,
        #dst=msg.dst_alias,
    #))
    conn.send(msg)

    responses = conn.receive_multi(.2)
    for response in responses:
        msg = messages.parse_frame(response)
        #if isinstance(msg, messages.DatagramTypeNotAccepted):
            #logger.info('Received expected {0} message: {1}'.format(
                #msg.__class__.__name__, response))
        #if not isinstance(msg, messages.DatagramTypeNotAccepted):
            #logger.error('Received unexpected {0} message: {1}'.format(
                #msg.__class__.__name__, response))
@nodetest
def test_cdi_2(conn, config):
    ''''''


    dst_alias, src_alias = config['dst_alias'], config['src_alias']


    for data in xrange(0, 0x01):

        send_datagram_message(src_alias, dst_alias, data,  conn)

        datagram_reply(src_alias, dst_alias, conn)
