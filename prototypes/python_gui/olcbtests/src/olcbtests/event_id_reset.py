'''
Sends reset Datagram message

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def user_clear(conn, config):
    '''Sends User Clear Datagram
    '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']


    msg = messages.StartDatagramFrame(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body = '200100000000EE55'
    )
    conn.send(msg)
    
    msg = messages.DatagramLast(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body = '33CC'
    )
    conn.send(msg)
    responses = conn.receive_multi(.5)
    logger.info('Received {0} event messages'.format(len(responses)))

    msg = messages.ResetDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
    )
    conn.send(msg)
    responses = conn.receive_multi(10)
    logger.info('Received {0} event messages'.format(len(responses)))


    
@nodetest
def mfr_clear(conn, config):
    '''Sends User Clear Datagram
    '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']


    msg = messages.StartDatagramFrame(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body = '2001000000000000'
    )
    conn.send(msg)
    
    msg = messages.DatagramLast(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body = '0000'
    )
    conn.send(msg)
    responses = conn.receive_multi(.5)
    logger.info('Received {0} event messages'.format(len(responses)))

    msg = messages.ResetDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
    )
    conn.send(msg)
    responses = conn.receive_multi(10)
    logger.info('Received {0} event messages'.format(len(responses)))