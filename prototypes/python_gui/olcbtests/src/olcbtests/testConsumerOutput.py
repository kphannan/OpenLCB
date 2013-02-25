'''
   Sends the Identify Events Addressed message
   Receives Identified Consumers
   Sends each Producer/Consumer Event to test output indications
   @author: Tim Hatch
'''
import openlcb.can
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def test_consumer_output(conn, config):
    '''
       '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']

    msg = messages.IdentifyEventsAddressed(
        src_alias=src_alias,
        dst_alias=dst_alias,
    )
    conn.send(msg)

    responses = conn.receive_multi(.5)
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
        #logger.info('Sending Consumer output {0}'.format(response.__class__.__name__, response.event_id))

        conn.send(msg)
        logger.info('Should not receive response')

        response2 = conn.receive_multi(.5)
        for frame in response2:
            response =  messages.parse_frame(frame)

            logger.error('Received unexpected response {0} from {1}'.format(response.__class__.__name__, response.event_id))
            
    
