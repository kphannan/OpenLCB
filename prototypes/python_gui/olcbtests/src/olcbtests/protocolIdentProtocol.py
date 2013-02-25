
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def protocol_ident_protocol(conn, config):
    ''''''

    dst_alias, src_alias = config['dst_alias'], config['src_alias']

    msg = messages.PIPMessageSend(
        src_alias=src_alias,
        dst_alias=dst_alias,
        )

    logger.info('Sending Protocol Identification Protocol message from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

    responses = conn.receive_multi()
    for response in responses:
        msg = messages.parse_frame(response)
        if isinstance(msg, messages.PIPMessageRecv):
            logger.info('Received expected {0} message: {1}'.format(
                msg.__class__.__name__, response))
        if not isinstance(msg, messages.PIPMessageRecv):
            logger.error('Received unexpected {0} message: {1}'.format(
                msg.__class__.__name__, response))
    logger.info('Received {0} event messages'.format(len(responses)))



    ''''''


