'''
Send IdentifyProducers message

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def identify_producers(conn, config):
    '''Sends Identify Events Addressed
       Receives Identified Producers
       then resends the Identified Producers and waits for correct response
    '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']

    #conn._socket.settimeout(.025) #commented out because does not work on serial com
    msg = messages.IdentifyEventsAddressed(
        src_alias=src_alias,
        dst_alias=dst_alias,
    )
    conn.send(msg)
    responses = conn.receive_multi()
    logger.info('Received {0} event messages'.format(len(responses)))
    for frame in responses:
        response = messages.parse_frame(frame)
        if not isinstance(response, messages.ProducerIdentified):
           continue
        logger.debug('Received {0} message from {1}'.format(response.__class__.__name__, response.event_id))
        msg = messages.IdentifyProducers(
            src_alias=src_alias,
            event_id=response.event_id
        )

        conn.send(msg)
        response2 = conn.receive_one()
        if isinstance(response, messages.ProducerIdentified):
            logger.info('Received expected {0} message from {1}'.format(response.__class__.__name__, response.event_id))
        if not isinstance(response, messages.ProducerIdentified):
            logger.warning('Received unexpected {0} message from {1}'.format(response.__class__.__name__, response.event_id))

    #conn._socket.settimeout(1.0) #commented out because does not work on serial com

@nodetest
def identify_producers_unknown(conn, config):
    '''Sends Identify Events Addressed
       Receives Identified Producers
       then resends the Identified Producers and waits for correct response
    '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']

    #conn._socket.settimeout(.025) #commented out because does not work on serial com
    msg = messages.IdentifyEventsAddressed(
        src_alias=src_alias,
        dst_alias=dst_alias,
    )
    conn.send(msg)

    responses = conn.receive_multi()
    logger.info('Received {0} event messages'.format(len(responses)))
    for frame in responses:
        response = messages.parse_frame(frame)
        if not isinstance(response, messages.IdentifyProducersUnknown):
           continue

        logger.debug('Received {0} message from {1}'.format(response.__class__.__name__, response.event_id))
        msg = messages.IdentifyProducers(
            src_alias=src_alias,
            event_id=response.event_id
        )

        conn.send(msg)
        response2 = conn.receive_one()
        if isinstance(response, messages.IdentifyProducersUnknown):
            logger.info('Received expected {0} message from {1}'.format(response.__class__.__name__, response.event_id))
        if not isinstance(response, messages.IdentifyProducersUnknown):
            logger.warning('Received unexpected {0} message from {1}'.format(response.__class__.__name__, response.event_id))

    #conn._socket.settimeout(1.0) #commented out because does not work on serial com