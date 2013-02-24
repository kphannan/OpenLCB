'''
Send node verification addressed message

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def verify_node_addressed(conn, config):
    '''Sends the VerifyNode Addressed command to connected node'''

    dst_alias, src_alias = config['dst_alias'], config['src_alias']

    msg = messages.VerifiedNodeAddressed(
        src_alias=src_alias,
        dst_alias=dst_alias,
    )
    logger.info('Sending Verify Node Addressed from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))

    conn.send(msg)
    responses = conn.receive_multi()
    for response in responses:
        msg = messages.parse_frame(response)
        if not isinstance(msg, messages.VerifiedNodeIDNumber):
            continue

        logger.info('Received Node alias {0} and Node ID {1}'.format(msg.src_alias, msg.node_id))
    #else:
        #logger.error('Received unexpected {0} message: {1}'.format(
                #msg.__class__.__name__, response))
                

