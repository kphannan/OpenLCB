from olcbtests.messages import can
from olcbtests.tests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def verify_node_global(conn, config):
    '''Sends the VerifyNode command to the whole bus'''

    src_alias = config['src_alias']

    msg = can.VerifyNodeIDNumberSimple(src_alias)
    logger.info('Sending VerifyNode message from {src}'.format(
        src=msg.src_alias
    ))
    conn.send(msg)
    response = conn.receive()

    response_msg = can.VerifiedNodeIDNumber.from_string(response)
    logger.info('Got response from node {alias}'.format(
        alias=response_msg.src_alias
    ))
