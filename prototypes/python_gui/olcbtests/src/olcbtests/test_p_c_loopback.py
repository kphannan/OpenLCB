'''
   Sends the Identify Events Addressed message
   Receives Identified Consumers
   Sends each Producer/Consumer Event to test output indications
   waits for a response from a producer.
   @author: Tim Hatch
'''
import openlcb.can
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def test_p_c_loopback(conn, config):
    '''This test require connections from the output of a consumer
       to the input of a producer.
       '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']

    msg = messages.IdentifyEventsAddressed(
        src_alias=src_alias,
        dst_alias=dst_alias,
    )
    conn.send(msg)

    responses = conn.receive_multi(1)
    logger.info('Received {0} event messages'.format(len(responses)))
    for frame in responses:
        response = messages.parse_frame(frame)
        if not isinstance(response, messages.ConsumerIdentifiedValid):
            if not isinstance(response, messages.ConsumerIdentifiedUnknown):
               continue
        logger.debug('Received {0} message from {1}'.format(response.__class__.__name__, response.event_id))
        msg = messages.SendProducerConsumerEvent(
            src_alias=src_alias,
            event_id=response.event_id
        )
        logger.info('Sending Consumer output {0}'.format(response.__class__.__name__, response.event_id))

        conn.send(msg)

        response2 = conn.receive_multi(.2)
        for frame in response2:
            response =  messages.parse_frame(frame)
            if isinstance(response, messages.SendProducerConsumerEvent):
                logger.info('Received expected Producer Consumer Event')
            

