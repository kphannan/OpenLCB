import openlcb.can
from openlcb.can import messages
from olcbtests.util import nodetest
import logging

logger = logging.getLogger(__name__)

def start_datagram(src_alias, dst_alias, conn):
    msg = messages.StartDatagramFrame(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='2041000000'
    )
    logger.info('Sending start Datagram from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

def last_datagram(src_alias, dst_alias, conn):
    msg = messages.DatagramLast(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='0008'
    )
    logger.info('Sending final Datagram from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

def datagram_reply(src_alias, dst_alias, conn):
    msg = messages.DatagramReceived(
        src_alias=src_alias,
        dst_alias=dst_alias
        )
    logger.info('Sending datagram reply from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

def send_arm_frame(src_alias, conn):
    msg = messages.AliasMapRequest(
         src_alias=src_alias
         )
    logger.info('Sending ARM from {src}'.format(
         src=msg.src_alias,
    ))
    conn.send(msg)
def send_amd_frame(src_alias, conn):
    msg = messages.AliasMapDefinition(
         src_alias=src_alias
         )
    logger.info('Sending AMD from {src}'.format(
         src=msg.src_alias,
    ))
    conn.send(msg)


def receive_response(responses):
    for response in responses:
        msg = messages.parse_frame(response)
        expected_responses = (
        messages.DatagramReceived,
        messages.DatagramConfiguration,
        messages.GeneralDatagram,
        messages.DatagramLast,
        messages.StartDatagramFrame,
        messages.DatagramRejected,
        messages.DatagramLast,
        messages.DatagramGeneral_1,
        messages.DatagramGeneral_2,
        messages.DatagramTypeNotAccepted
        )

        if isinstance(msg, expected_responses):
            logger.info('Received expected {0} message: {1}'.format(
                msg.__class__.__name__, response))
        if not isinstance(msg, expected_responses):
            logger.error('Received unexpected {0} message: {1}'.format(
                msg.__class__.__name__, response))


@nodetest
def test_datagram(conn, config):
    ''''''

    dst_alias, src_alias = config['dst_alias'], config['src_alias']
    logger.info('Testing Two Datagram Segments')
    start_datagram(src_alias, dst_alias, conn)
    last_datagram(src_alias, dst_alias, conn)

    responses = conn.receive_multi()
    logger.info('Received {0} Datagram messages'.format(len(responses)))
    receive_response(responses)

    datagram_reply(src_alias, dst_alias, conn)

    logger.info('test two segments with extraneous one interposed')
    start_datagram(src_alias, dst_alias, conn)
    new_dst_alias = (~src_alias+2)&0xFFF
    msg = messages.StartDatagramFrame(
        src_alias=src_alias,
        dst_alias=new_dst_alias+2,
        body='2041000000'
    )

    logger.info('Sending start Datagram from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

    last_datagram(src_alias, dst_alias, conn)
    
    responses = conn.receive_multi()
    logger.info('Received {0} Datagram messages'.format(len(responses)))
    receive_response(responses)

    datagram_reply(src_alias, dst_alias, conn)

    logger.info('test two segments with another datagram interposed')
    start_datagram(src_alias, dst_alias, conn)
    newalias = (~src_alias)&0xFFF
    if newalias == dst_alias:
	newalias = (newalias - 1)&0xFFF;
    msg = messages.StartDatagramFrame(
        src_alias=newalias,
        dst_alias=dst_alias,
        body='20410000000008'
    )

    logger.info('Sending start Datagram from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

    last_datagram(src_alias, dst_alias, conn)

    responses = conn.receive_multi()
    logger.info('Received {0} Datagram messages'.format(len(responses)))
    receive_response(responses)

    datagram_reply(src_alias, dst_alias, conn)


    logger.info('send NAK to response')
    msg = messages.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='20410000000001'
    )

    logger.info('Sending start Datagram from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)


    responses = conn.receive_multi()
    logger.info('Received {0} Datagram messages'.format(len(responses)))
    receive_response(responses)

    datagram_reply(src_alias, dst_alias, conn)



    msg = messages.DatagramRejectedBufferFull(
        src_alias=src_alias,
        dst_alias=dst_alias,
        #body='20420000000001'
    )

    logger.info('Sending start from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

    responses = conn.receive_multi()
    logger.info('Received {0} Datagram messages'.format(len(responses)))
    receive_response(responses)

    datagram_reply(src_alias, dst_alias, conn)
    
    logger.info('test recovery from AMR (node failure) after partial transmission of datagram')
    start_datagram(src_alias, dst_alias, conn)
    send_arm_frame(src_alias, conn)
    msg = messages.DatagramRejectedBufferFull(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='0000000008'
    )

    logger.info('Sending start from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

    responses = conn.receive_multi()
    logger.info('Received {0} Datagram messages'.format(len(responses)))
    receive_response(responses)
    
    datagram_reply(src_alias, dst_alias, conn)

    logger.info('test recovery from AMD (node failure) after partial transmission of datagram')
    msg = messages.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='000000'
    )
    logger.info('Sending last Datagram from {src}'.format(
        src=msg.src_alias,

    ))
    conn.send(msg)
    msg = messages.AliasMapDefinition(
        src_alias=src_alias,

    )
    conn.send(msg)
    msg = messages.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='20410000000008'
    )
    logger.info('Sending last Datagram from {src}'.format(
        src=msg.src_alias,

    ))
    conn.send(msg)
    responses = conn.receive_multi()
    logger.info('Received {0} Datagram messages'.format(len(responses)))
    receive_response(responses)
    datagram_reply(src_alias, dst_alias, conn)
    
    logger.info('test that AMR, AMD from other nodes doesnt interfere')
    msg = messages.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='20'
    )
    logger.info('Sending last Datagram from {src}'.format(
        src=msg.src_alias,

    ))
    conn.send(msg)
    send_arm_frame(src_alias, conn)
    send_amd_frame(src_alias, conn)
    msg = messages.GeneralDatagram(
        src_alias=src_alias,
        dst_alias=dst_alias,
        body='20410000000008'
    )
    logger.info('Sending last Datagram from {src}'.format(
        src=msg.src_alias,

    ))
    conn.send(msg)
    responses = conn.receive_multi()
    logger.info('Received {0} Datagram messages'.format(len(responses)))
    receive_response(responses)
    datagram_reply(src_alias, dst_alias, conn)