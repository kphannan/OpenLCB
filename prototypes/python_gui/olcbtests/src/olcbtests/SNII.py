'''
Send Send Simple Node Identification Information message

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def simple_node_ident_info(conn, config):
    ''''''

    src_alias, dst_alias , node_id =  config['src_alias'], config['dst_alias'] , config['node_id']

    msg = messages.SimpleNodeIdentInfo(
        src_alias=src_alias,
        dst_alias=dst_alias,

    )
    logger.info('Sending Simple Node Ident Info from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

    responses = conn.receive_multi()
    for response in responses:
        msg = messages.parse_frame(response)
        if isinstance(msg, messages.SimpleNodeIdentInfoAck):
            logger.info('Received expected {0} message: {1}'.format(
                msg.__class__.__name__, response))
        if not isinstance(msg, messages.SimpleNodeIdentInfoAck):
            logger.error('Received expected {0} message: {1}'.format(
                msg.__class__.__name__, response))
    logger.info('Received {0} messages'.format(len(responses)))



    msg = messages.DatagramReceived(
        src_alias=src_alias,
        dst_alias=dst_alias,

    )

    logger.info('Sending Message Received from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

