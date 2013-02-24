from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def verify_node_global(conn, config):
    '''Sends the VerifyNode command to the whole bus'''

    src_alias = config['src_alias']#, config['dst_id']

    msg = messages.VerifyNodeIDNumberSimple(src_alias=src_alias)
    logger.info('Sending VerifyNode message from {src}'.format(
        src=msg.src_alias
    ))
    conn.send(msg)
    responses = conn.receive_multi()
    logger.info('Received response from {0} node(s)'.format(len(responses)))

    for frame in responses:
        response = messages.parse_frame(frame)
        if not isinstance(response, messages.VerifiedNodeIDNumber):
           continue

        logger.info('Received {0} message from Node alias -- {1}'.format(response.__class__.__name__, response.src_alias))
