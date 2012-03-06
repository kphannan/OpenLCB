from olcbtests.tests import discover_node, nodeverification, general
import argparse
import comms
import logging

root_logger = logging.getLogger()

def main():
    '''Main command-line interface for OpenLCBTests'''

    aparser = argparse.ArgumentParser()
    aparser.add_argument('commtype', choices=['ethernet'],
        help='Specifies the communication method'
    )
    aparser.add_argument('--src-alias', default=0xAAA,
        help='Specify the alias of the sending node (i.e. this computer)'
    )
    
    eth_args = aparser.add_argument_group('ethernet',
        'Arguments for Ethernet-to-CAN communication')
    eth_args.add_argument('hostname',
        help='Hostname (or IP address) of the Eth2CAN node')
    eth_args.add_argument('--port', '-p', type=int, default=23,
        help='TCP port of the Eth2CAN node (default: 23)')

    args = aparser.parse_args()

    # Set up console logging
    # TODO: Get logging parameters from cli/config file
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.DEBUG)
    console_handler.setFormatter(logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    ))
    root_logger.setLevel(logging.DEBUG)
    root_logger.addHandler(console_handler)

    if args.commtype == 'ethernet':
        conn = comms.EthernetConnection(args.hostname, args.port)
    #elif arguments.commtype == 'serial':
    #    conn = comms.SerialConnection()
   
    with conn as c:
        # Run the tests!
        # TODO: Dynamically discover tests to run
        dst_alias, dst_id = discover_node(conn, args.src_alias)
        nodeverification.verify_node_global(c, args.src_alias)
        general.read_config_memory(c, args.src_alias, dst_alias)

    conn.close()
