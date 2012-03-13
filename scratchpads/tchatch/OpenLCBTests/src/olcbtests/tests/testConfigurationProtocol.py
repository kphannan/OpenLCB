from olcbtests.messages import can
from olcbtests.tests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def test_configuration_protocol(conn, config):
    ''''''

    dst_alias, src_alias = config['dst_alias'], config['src_alias']

    msg = can.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='2084FF'
    )
    logger.info('Sending configuration protocol from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

    #response = conn.receive()
    responses = conn.receive_multi()
    logger.info('Received {0} Datagram messages'.format(len(responses)))
    

    ''''''

    dst_alias, src_alias = config['dst_alias'], config['src_alias']

    msg = can.DatagramReceived(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body=hex(0x4C).upper()[2:]
    )
    logger.info('Sending Datagram Received from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)