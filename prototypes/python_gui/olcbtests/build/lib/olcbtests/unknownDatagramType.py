'''
Send Unknown Datagram Type message

@author: Tim Hatch
'''
import openlcb.can
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

known = [0x01, 0x02, 0x20, 0x21, 0x30]

logger = logging.getLogger(__name__)
def send_datagram_message(mti, src_alias, dst_alias, conn):
    msg = messages.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='{0:>02X}'.format(mti)
        )
    logger.debug('Sending Unknown Datagram Type message from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

    responses = conn.receive_multi(.03)
    for response in responses:
        msg = messages.parse_frame(response)
        if isinstance(msg, messages.DatagramTypeNotAccepted):
            logger.info('Received expected {0} message: {1}'.format(
                msg.__class__.__name__, response))
        if not isinstance(msg, messages.DatagramTypeNotAccepted):
            logger.error('Received unexpected {0} message: {1}'.format(
                msg.__class__.__name__, response))
@nodetest
def unknown_datagram_type(conn, config):
    ''''''


    dst_alias, src_alias = config['dst_alias'], config['src_alias']


    for n in xrange(0, 0x100):
        if n in known : continue
        send_datagram_message(n, src_alias, dst_alias, conn)

