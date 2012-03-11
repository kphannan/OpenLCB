from olcbtests.messages import can
from olcbtests.tests.util import nodetest
import logging

logger = logging.getLogger(__name__)

@nodetest
def read_config_memory(conn, config):
    ''''''

    dst_alias, src_alias = config['dst_alias'], config['src_alias']

    msg = can.GeneralDatagram(src_alias, dst_alias, '20620000000040')
    logger.info('Sending config memory read from {src} to {dst}'.format(
        src=msg.src_alias,
        dst=msg.dst_alias,
    ))
    conn.send(msg)

    response = conn.receive()
