'''
Sends reset Datagram message

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def reset_datagram(conn, config):
    '''Sends reset Datagram
    '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']

    #conn._socket.settimeout(5) #commented out because does not work on serial com
    msg = messages.ResetDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
    )
    conn.send(msg)
    responses = conn.receive_multi(1.9)
    logger.info('Received {0} event messages'.format(len(responses)))
    #conn._socket.settimeout(.3) #commented out because does not work on serial com