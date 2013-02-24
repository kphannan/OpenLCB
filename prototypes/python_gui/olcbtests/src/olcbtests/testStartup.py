'''
Test Startup sequence
Sends reset Datagram message then listens for the correct responses

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

def received_response(responses):
    for response in responses:
        msg = messages.parse_frame(response)
        expected_responses = (
        messages.DatagramReceived,
        messages.CidMessage1, 
        messages.CidMessage2,
        messages.CidMessage3, 
        messages.CidMessage4,
        messages.ReserveID,  
        messages.AliasMapRequest,
        messages.AliasMapDefinition,
        messages.InitializationComplete, 
        messages.ConsumerIdentifiedValid, 
        messages.ProducerIdentified,
        messages.ConsumerIdentifiedUnknown,
        messages.IdentifyProducersUnknown,
        messages.SendProducerConsumerEvent)

        if isinstance(msg, expected_responses):
                    logger.info('Received expected {0} message: {1}'.format(
                        msg.__class__.__name__, response))

        if not isinstance(msg, expected_responses):
                    logger.error('Received unexpected {0} message: {1}'.format(
                        msg.__class__.__name__, response))
@nodetest
def test_start_up(conn, config):
    '''Sends reset Datagram
    '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']

    msg = messages.ResetDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
    )
    conn.send(msg)
    responses = conn.receive_multi(1.9)
    logger.info('Received {0} event messages'.format(len(responses)))
    received_response(responses)
    logger.info('Received {0} event messages'.format(len(responses)))
