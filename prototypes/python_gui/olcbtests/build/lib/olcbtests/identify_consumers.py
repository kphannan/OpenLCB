'''
Send IdentifyConsumers message

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def identify_consumers_valid(conn, config):
    '''Sends Identify Events Addressed
       Receives Identified Consumers
       then resends the Identified Consmers and waits for correct response
    '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']


    msg = messages.IdentifyEventsAddressed(
        src_alias=src_alias,
        dst_alias=dst_alias
    )
    conn.send(msg)
    responses = conn.receive_multi()
    for frame in responses:
        response = messages.parse_frame(frame)
        if not isinstance(response, messages.ConsumerIdentifiedValid):
           continue
        logger.debug('Received {0} message from {1}'.format(response.__class__.__name__, response.event_id))
        msg = messages.IdentifyConsumers(
            src_alias=src_alias,
            event_id=response.event_id
        )
        conn.send(msg)
        response2 = conn.receive_multi(.2)
        if isinstance(response, messages.ConsumerIdentifiedValid):
            logger.info('Received expected {0} message from {1}'.format(response.__class__.__name__, response.event_id))

        if not isinstance(response, messages.ConsumerIdentifiedValid):
            logger.warning('Received unexpected {0} message from {1}'.format(response.__class__.__name__, response.event_id))



@nodetest
def identify_consumers_unknown(conn, config):
    '''Sends Identify Events Addressed
       Receives Identified Consumers
       then resends the Identified Consmers and waits for correct response
    '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']


    msg = messages.IdentifyEventsAddressed(
        src_alias=src_alias,
        dst_alias=dst_alias
    )
    conn.send(msg)
    responses = conn.receive_multi(.2)
    for frame in responses:
        response = messages.parse_frame(frame)
        if not isinstance(response, messages.ConsumerIdentifiedUnknown):
           continue
        logger.debug('Received {0} message from {1}'.format(response.__class__.__name__, response.event_id))
        msg = messages.IdentifyConsumers(
            src_alias=src_alias,
            event_id=response.event_id
        )
        conn.send(msg)
        response2 = conn.receive_one()
        if isinstance(response, messages.ConsumerIdentifiedUnknown):
            logger.info('Received expected {0} message from {1}'.format(response.__class__.__name__, response.event_id))
        if not isinstance(response, messages.ConsumerIdentifiedUnknown):
            logger.error('Received unexpected {0} message from {1}'.format(response.__class__.__name__, response.event_id))
    