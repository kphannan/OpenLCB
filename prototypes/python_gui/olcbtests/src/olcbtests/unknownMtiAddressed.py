'''
Send Unknown MTI Addressed message

@author: Tim Hatch
'''
import openlcb.can
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

known = [0x488, 0x068, 0x0A8, 0x828, 0x628, 0x968, 0xDE8, 0xA08, 0xA28, 0xA48, 0xCC8, 0x868, 0x888, 0x8A8]
ver=0
logger = logging.getLogger(__name__)
class MtiMessage(openlcb.can.AddressedMessage):
    MTI = 0x19 + (ver)
def send_mtiaddress_message(mti, src_alias, dst_alias, conn):
    msg = MtiMessage(
        src_alias=mti,
        dst_alias=dst_alias,
        #body='{0:>02X}'.format(mti)
        )
    logger.info('Sending unknown MTI message from {src}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)
    responses = conn.receive_multi(.01)
    for response in responses:
        msg = messages.parse_frame(response)
        if isinstance(msg, messages.InteractionRejected):
            logger.info('Received expected {0} message: {1}'.format(
                msg.__class__.__name__, response))
        if not isinstance(msg, messages.InteractionRejected):
            logger.error('Received unexpected {0} message: {1}'.format(
                msg.__class__.__name__, response))

@nodetest
def unknown_mti_addressed(conn, config):
    ''''''


    src_alias, dst_alias = config['src_alias'], config['dst_alias']

    #conn._socket.settimeout(.025) #commented out does not work with serial comm
    for ver in range(0,4095) :
        if ver in known : continue
        if (ver&0x08) == 0 : continue

    for mti in xrange(0, 0x4096):
        if mti in known : continue
        if (ver&0x08) == 0 : continue
        send_mtiaddress_message(mti, src_alias, dst_alias, conn)
    #conn._socket.settimeout(1.0)


