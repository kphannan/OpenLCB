'''
Send IdentifyEventsAddressed message

@author: Tim Hatch
'''
from olcbtests.messages import can
from olcbtests.tests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def identify_events_addressed(conn, config):
    '''Sends the events addressed command to the whole bus'''

    dst_alias, src_alias = config['dst_alias'], config['src_alias']

    msg = can.IdentifyEventsAddressed(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body=hex(0x2B).upper()[2:]
    )
    logger.info('Sending Idnetify Events addressed message from {src}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)
    responses = conn.receive_multi()
    logger.info('Received {0} event messages'.format(len(responses)))

@nodetest
def identify_events_global(conn, config):
    '''Sends the events Global command to the whole bus'''

    src_alias = config['src_alias']

    msg = can.IdentifyEventsGlobal(src_alias=src_alias)
    logger.info('Sending Idnetify Events Global message from {src}'.format(
        src=msg.src_alias,
    ))
    conn.send(msg)
    responses = conn.receive_multi()
    logger.info('Received {0} event messages'.format(len(responses)))
    for msg in can.ConsumerIdentified.from_sequence(responses):
        logger.debug('Got event ID 0x{0:x}'.format(msg.event_id))
