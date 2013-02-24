'''
Send test Alias Map Enquiry message

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def alias_map_enquiry(conn, config):
    '''Sends the events addressed command to a connected node on bus'''

    src_alias, dst_alias, node_id =  config['src_alias'], config['dst_alias'], config['dst_id']

    msg = messages.AliasMapEquiry(
        src_alias=src_alias,
        node_id=node_id
        )
    logger.info('Sending Alias Map Equiry  from {src}'.format(
        src=msg.src_alias,
    ))
    #conn._socket.settimeout(1.0)
    conn.send(msg)
    responses = conn.receive_multi()
    for response in responses:
        msg = messages.parse_frame(response)
        if isinstance(msg, messages.AliasMapDefinition):
           logger.error('Received expected {0} message: {1}'.format(
                msg.__class__.__name__, response))
        if not isinstance(msg, messages.AliasMapDefinition):
           logger.error('Received unexpected {0} message: {1}'.format(
                msg.__class__.__name__, response))
    logger.info('Received {0} event messages'.format(len(responses)))

    msg = messages.AliasMapEquiry(
        src_alias=src_alias,
        node_id=[0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
        )
    logger.info('Sending Alias Map Equiry  from {src}'.format(
        src=msg.src_alias,
    ))
    #conn._socket.settimeout(1.0)
    conn.send(msg)

    logger.info('expect no reponse')
    responses = conn.receive_multi()
    for response in responses:
            response =  messages.parse_frame(response)

            logger.error('Received unexpected response')
    logger.info('Received {0} event messages'.format(len(responses)))