'''
Send IdentifyEventsAddressed message

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def identify_events_addressed(conn, config):
    '''Sends the events addressed command to the whole bus'''

    dst_alias, src_alias = config['dst_alias'], config['src_alias']

    msg = messages.IdentifyEventsAddressed(
        src_alias=src_alias,
        dst_alias=dst_alias,
    )
    logger.info('Sending Identify Events addressed message from {src}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)
    responses = conn.receive_multi()
    logger.info('Received {0}  addressed event messages'.format(len(responses)))
    for response in responses:
        msg = messages.parse_frame(response)
        if isinstance(msg, messages.ConsumerIdentifiedValid):
            logger.debug('Got consumer event ID {0}--from source alias {1}'.format(msg.event_id, msg.src_alias))
        elif isinstance(msg, messages.ConsumerIdentifiedUnknown):
            logger.debug('Got consumer unknown event ID {0}--from source alias {1}'.format(msg.event_id, msg.src_alias))
        elif isinstance(msg, messages.ProducerIdentified):
            logger.debug('Got producer event ID {0}--from source alias {1}'.format(msg.event_id, msg.src_alias))
        elif isinstance(msg, messages.IdentifyProducersUnknown):
            logger.debug('Got consumer unknown event ID {0}--from source alias {1}'.format(msg.event_id, msg.src_alias))
        else:
            logger.warning('Received unexpected {0} message: {1}'.format(
                msg.__class__.__name__, response))


@nodetest
def identify_events_global(conn, config):
    '''Sends the events Global command to the whole bus'''

    src_alias = config['src_alias']

    msg = messages.IdentifyEventsGlobal(src_alias=src_alias)
    logger.info('Sending Identify Events Global message from {src}'.format(
        src=msg.src_alias,
    ))
    conn.send(msg)
    responses = conn.receive_multi()
    logger.info('Received {0} event messages'.format(len(responses)))
    for response in responses:
        msg = messages.parse_frame(response)
        if isinstance(msg, messages.ConsumerIdentifiedValid):
            logger.debug('Got consumer event ID {0}--from source alias {1}'.format(msg.event_id, msg.src_alias))
        elif isinstance(msg, messages.ConsumerIdentifiedUnknown):
            logger.debug('Got consumer unknown event ID {0}--from source alias {1}'.format(msg.event_id, msg.src_alias))
        elif isinstance(msg, messages.ProducerIdentified):
            logger.debug('Got producer event ID {0}--from source alias {1}'.format(msg.event_id, msg.src_alias))
        elif isinstance(msg, messages.IdentifyProducersUnknown):
            logger.debug('Got producer unknown event ID {0}--from source alias {1}'.format(msg.event_id, msg.src_alias))

        else:
            logger.warning('Received unexpected {0} message: {1}'.format(
                msg.__class__.__name__, response))
