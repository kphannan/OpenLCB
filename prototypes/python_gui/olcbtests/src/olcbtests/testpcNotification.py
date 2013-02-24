'''
Send IdentifyConsumers message

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def test_pc_notification(conn, config):
    '''Sends the events Global command to the whole bus'''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']

    #conn._socket.settimeout(1.0) #commented out because does not work on serial com
    msg = messages.IdentifyEventsAddressed(
        src_alias=src_alias,
        dst_alias=dst_alias,
        )
    conn.send(msg)
    responses = conn.receive_multi()
    logger.info('Received {0} event messages'.format(len(responses)))

    for frame in responses:
        response = messages.parse_frame(frame)
        logger.debug('Received {0} message from {1}'.format(response.__class__.__name__, response.event_id))
    #conn._socket.settimeout(1.0) #commented out because does not work on serial com

