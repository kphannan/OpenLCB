from olcbtests.messages import can
import logging

logger = logging.getLogger(__name__)

def verify_node_global(conn, src_alias):
    '''Sends the VerifyNode command to the whole bus'''

    msg = can.VerifyNodeIDNumberSimple(src_alias)
    logger.info('Sending VerifyNode message from {src}'.format(
        src=msg.src_alias
    ))
    conn.send(msg)
    response = conn.receive()

    response_msg = can.VerifiedNodeIDNumber.from_string(response)
    logger.info('Got response from node {alias}'.format(
        alias=response_msg.src_alias
    ))

def verify_node_addressed(conn, addr):
    '''Sends the VerifyNode command to the specified node
    
    :param CANConnection conn: TODO
    :param int addr: ??
    '''
