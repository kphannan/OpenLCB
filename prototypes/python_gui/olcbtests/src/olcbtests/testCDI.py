'''
Read and check the Configuration Definition Information

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def test_config_def_info(conn, config):
    ''''''

    dst_alias, src_alias = config['dst_alias'], config['src_alias']

    msg = messages.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='20430000000010'
    )
    #logger.info('Sending config memory read from {src} to {dst}'.format(
        #src=msg.src_alias,
       # dst=msg.dst_alias,
    #))
    conn.send(msg)
    responses = conn.receive_multi()
    #response = conn.receive()

    #logger.info('Received {0} Datagram messages'.format(len(responses)))
    

    '''
    Except reply to sending node

    '''

    #dst_alias, src_alias = config['dst_alias'], config['src_alias']

    msg = messages.DatagramReceived(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='4C'
    )
    #logger.info('Sending Datagram Received from {src} to {dst}'.format(
        #src=msg.src_alias,
        #dst=msg.dst_alias,
    #))
    conn.send(msg)

    msg = messages.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='20430000001010'
    )
    conn.send(msg)
    responses = conn.receive_multi()
    msg = messages.DatagramReceived(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='4C'
    )
    conn.send(msg)
    
    msg = messages.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='20430000002010'
    )
    conn.send(msg)
    responses = conn.receive_multi()
    msg = messages.DatagramReceived(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='4C'
    )
    conn.send(msg)
