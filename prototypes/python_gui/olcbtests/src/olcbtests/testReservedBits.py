'''
Send test Reserved Bits message

Test that various reserved bits are not being (incorrectly) checked

Relies on the node properly handling verifyNodeAddressed and verifyNodeGlobal
@author: Tim Hatch
'''
import openlcb.can
from openlcb.can import messages
from olcbtests.util import nodetest
import logging


class ReservedBitsMessage(openlcb.can.GlobalMessage, openlcb.can.NodeIDMessage):
    MTI = 0x088a7
    
class ReservedBitsMessage_(openlcb.can.AddressedMessage, openlcb.can.NodeIDMessage):
    MTI = 0x0e
    BODY_MTI=0x0a

logger = logging.getLogger(__name__)

def reserved_bits_global_node(src_alias, dst_id, conn):
    msg = ReservedBitsMessage(
        src_alias=src_alias,
        node_id=dst_id
        )
    logger.info('Sending reserved bits w/ verified global from {src}'.format(
        src=msg.src_alias,
    ))
    conn.send(msg)
    
def reserved_bits_no_node(src_alias, dst_id, conn):
    msg = ReservedBitsMessage(
        src_alias=src_alias,
        node_id=[]
        )
    logger.info('Sending reserved bits w/ no node id from {src}'.format(
        src=msg.src_alias,
    ))
    conn.send(msg)

def reserved_bits_incorrect_node(src_alias, dst_id, conn):
    msg = ReservedBitsMessage(
        src_alias=src_alias,
        node_id=[0x00,0x00,0x00,0x00,0x00,0x01]
        )
    logger.info('Sending reserved bits w/ incorrect node id from {src}'.format(
        src=msg.src_alias,
    ))
    conn.send(msg)

def reserved_bits_verified_node(src_alias, dst_alias, dst_id, conn):
    msg = ReservedBitsMessage_(
        src_alias=src_alias,
        dst_alias=dst_alias,
        node_id=dst_id
        )
    logger.info('Sending reserved bits w/ verified node id from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias
    ))
    conn.send(msg)

def reserved_bits_invalid_alias(src_alias, dst_alias, node_id, conn):
    msg = ReservedBitsMessage_(
        src_alias=src_alias,
        dst_alias=0xBBB,
        node_id=node_id
        )
    logger.info('Sending reserved bits w/ invalid alias from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias
    ))
    conn.send(msg)

def reserved_bits_incorrect_node(src_alias, node_id, conn):
    msg = ReservedBitsMessage(
        src_alias=src_alias,
        node_id=[0x00,0x00,0x00,0x00,0x00,0x01]
        )
    logger.info('Sending Reserved Bits w/ incorrect node id  from {src}'.format(
        src=msg.src_alias,
        ))
    conn.send(msg)

def receive_response(responses):
    for response in responses:
        msg = messages.parse_frame(response)
        if isinstance(msg, messages.VerifiedNodeIDNumber):
            logger.info('Received expected {0} message: {1}'.format(
                msg.__class__.__name__, response))
        if not isinstance(msg, messages.VerifiedNodeIDNumber):
            logger.error('Received unexpected {0} message: {1}'.format(
                msg.__class__.__name__, response))

def receive_response_two(responses):
    for response in responses:
        msg = messages.parse_frame(response)
        logger.error('Received unexpected {0} message: {1}'.format(
                msg.__class__.__name__, response))

@nodetest
def test_reserved_bits(conn, config):
    '''
    Test sends reserved bits Global: expect reply
         sends reserved bits with no source alias: expect reply
         sends reserved bits with verified node id: expect reply
         sends reserved bits with incorrect node alias: expect no reply
         sends reserved bits with incorrect node id: expect no reply
    '''
    src_alias, dst_alias, node_id = config['src_alias'], config['dst_alias'], config['dst_id']

    #conn._socket.settimeout(.2) #commented out does not work with serial comm
    #sends reserved bits Global: expect reply
    reserved_bits_global_node(src_alias, node_id, conn)
    responses = conn.receive_multi()
    receive_response(responses)
    #sends reserved bits with no source alias: expect reply
    reserved_bits_no_node(src_alias, node_id, conn)
    responses = conn.receive_multi()
    receive_response(responses)
    #sends reserved bits with verified node id: expect reply
    reserved_bits_verified_node(src_alias, dst_alias, node_id, conn)
    responses = conn.receive_multi()
    receive_response(responses)
    #sends reserved bits with incorrect node alias: expect no reply
    reserved_bits_invalid_alias(src_alias, dst_alias, node_id, conn)
    logger.info('expect no response!')
    responses = conn.receive_multi()
    receive_response_two(responses)
    #sends reserved bits with incorrect node id: expect no reply
    reserved_bits_incorrect_node(src_alias, node_id, conn)
    logger.info('expect no response!')
    responses = conn.receive_multi()
    receive_response_two(responses)
                
    #conn._socket.settimeout(1.0)