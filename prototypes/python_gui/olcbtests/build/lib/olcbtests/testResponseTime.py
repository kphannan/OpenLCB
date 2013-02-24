'''
Send Test response time of the node connect to the buss message.

@author: Tim Hatch
'''
from openlcb.can import messages
from olcbtests.util import nodetest
import logging
import time

logger = logging.getLogger(__name__)

@nodetest
def test_response_time(conn, config):
    '''Sends the events Global command to the whole bus
    Test the response time of the node that is connect to the bus.
    Only one node should be connected to the buss for this test.
    '''

    src_alias, dst_alias = config['src_alias'], config['dst_alias']

    #conn._socket.settimeout(.01) #commented out because does not work on serial com
    msg = messages.VerifyNodeIDNumberSimple(
        src_alias=src_alias,
        dst_alias=dst_alias,

    )
    conn.send(msg)
    start = time.time()
    r = conn.receive()
    end = time.time()
    print end-start
