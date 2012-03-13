'''
Send IdentifyProducers message

@author: Tim Hatch
'''
from olcbtests.messages import can
from olcbtests.tests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def identify_producers(conn, config):
    '''Sends the events Global command to the whole bus'''

    src_alias = config['src_alias']

    msg = can.IdentifyProducers(src_alias=src_alias)
    logger.info('Sending Idnetify Producers message from {src}'.format(
        src=msg.src_alias,
    ))
    conn.send(msg)
    response = conn.receive()