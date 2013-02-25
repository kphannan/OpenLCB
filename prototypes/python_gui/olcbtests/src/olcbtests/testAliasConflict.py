'''
Send no-response Global Alias conflict message

@author: Tim Hatch
'''
import openlcb.can
from openlcb.can import messages
from olcbtests.util import nodetest
import logging
import util
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
        messages.VerifiedNodeIDNumber)


        if isinstance(msg, expected_responses):
                    logger.info('Received expected {0} message: {1}'.format(
                        msg.__class__.__name__, response))

        if not isinstance(msg, expected_responses):
                    logger.error('Received unexpected {0} message: {1}'.format(
                        msg.__class__.__name__, response))
@nodetest
def alias_conflict(conn, config):
    '''Sends the events addressed command to the whole bus'''

    src_alias, dst_alias, node_id =  config['src_alias'], config['dst_alias'], config['node_id']
    #conn._socket.settimeout(.7) #commented out because does not work on serial com

    logger.info("check no-response global message with alias conflict")

    msg = messages.VerifyNodeIDNumberSimple(
        src_alias=dst_alias,
        body=(('0' * 11) + '1')
    )
    logger.info('Sending no-response global message with alias conflict message from {src}'.format(
        src=msg.src_alias,

    ))
    conn.send(msg)

    responses = conn.receive_multi()
    received_response(responses)
    #conn._socket.settimeout(.9) #commented out because does not work on serial com

    logger.info('Received {0} messages'.format(len(responses)))
    logger.info("check response-inducing global message with alias conflict")
    response = messages.parse_frame(responses[-1])
    msg = messages.VerifyNodeIDNumberSimple(
        src_alias=response.src_alias,
    )
    conn.send(msg)

    responses = conn.receive_multi()
    received_response(responses)
   # conn._socket.settimeout(1.0) #commented out because does not work on serial com

    logger.info("check addressed message with alias conflict")
    response = messages.parse_frame(responses[-1])
    msg = messages.VerifiedNodeAddressed(
        src_alias=response.src_alias,
        dst_alias=src_alias
    )
    logger.info('Sending check addressed message with alias conflict message from {src}'.format(
        src=msg.src_alias,
    ))
    conn.send(msg)

    responses = conn.receive_multi()
    received_response(responses)
    logger.info('Received {0} messages'.format(len(responses)))

    logger.info("check CheckID with alias conflict")
    response = messages.parse_frame(responses[-1])
    msg = messages.CidMessage1(
        src_alias=response.src_alias,


    )
    logger.info('Sending check CheckID with alias conflict message from {src}'.format(
        src=msg.src_alias,
    ))
    conn.send(msg)

    responses = conn.receive_multi()
    received_response(responses)
    logger.info('Received {0} messages'.format(len(responses)))
    
    logger.info("check ReserveID with aliasconflict")
    response = messages.parse_frame(responses[-1])
    msg = messages.ReserveID(
        src_alias=response.src_alias,


    )
    logger.info('Sending check ReserveID with alias conflict message from {src}'.format(
        src=msg.src_alias,
    ))
    conn.send(msg)

    responses = conn.receive_multi()
    received_response(responses)
    logger.info('Received {0} messages'.format(len(responses)))

    
    #conn._socket.settimeout(1.0) #commented out because does not work on serial com

