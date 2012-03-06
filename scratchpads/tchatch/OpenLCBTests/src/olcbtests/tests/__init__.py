from olcbtests.messages import can
import logging

logger = logging.getLogger(__name__)

def discover_node(conn, src_alias):
    '''Discover the attached node using a Verify Node ID Number message
    
    :returns tuple: A 2-tuple containing the node alias and node ID, in
       that order
    '''

    msg = can.VerifyNodeIDNumberSimple(src_alias)
    conn.send(msg)
    r = conn.receive()
    response = can.VerifiedNodeIDNumber.from_string(r)

    logger.info('Discovered node {alias} ({id})'.format(
        alias=response.src_alias,
        id=response.node_id
    ))

    return response.src_alias, response.node_id

