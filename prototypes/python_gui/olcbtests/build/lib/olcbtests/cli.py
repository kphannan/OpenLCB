from olcbtests import util
from openlcb import communication
import argparse
import logging
import sys

root_logger = logging.getLogger()

def main():
    '''Main command-line interface for OpenLCBTests'''

    aparser = argparse.ArgumentParser()
    aparser.add_argument(
       'commtype', nargs='?', choices=['ethernet', 'serial'],
       default='ethernet', help='Specifies the communication method'
    )
    aparser.add_argument(
        'destination',
        help='Specify the network hostname OR serial communication port')
    aparser.add_argument(
        '--src-alias', default=0xAAA,
        type=lambda x: int(x, 16),
        help='Specify the hex alias of the sending node (i.e. this computer)'
    )
    aparser.add_argument(
        '--verbose', '-v', action='count', default=0,
        help='Set verbosity level (can be repeated to increase output)'
    )
    aparser.add_argument(
        '--test', '-t', action='append', metavar='TEST_NAME',
        help='Run a specific test (can be specified more than once'
    )
    aparser.add_argument(
        '--output-file', '-o', type=argparse.FileType('w'),
        help='Write log to OUTPUT_FILE'
    )
    aparser.add_argument(
        '--log-level', metavar='LEVEL', default='DEBUG',
        help='Only write message more at LEVEL or higher to the log'
    )

    eth_args = aparser.add_argument_group('ethernet',
        'Arguments for Ethernet-to-CAN communication')
    eth_args.add_argument('--port', '-p', type=int, default=23,
        help='TCP port of the Eth2CAN node (default: 23)')

    serial_args = aparser.add_argument_group('serial',
        'Argumments for Serial-to-CAN communication')
    serial_args.add_argument('--speed', '-s', default=500000,
        help='speed of serial port (default 500k)')

    args = aparser.parse_args()
    if args.commtype == 'serial':
        try:
            args.destination = int(args.destination)
        except ValueError:
            aparser.error('Destination must be a valid serial port number')
        

    root_logger.setLevel(logging.DEBUG)

    default_formatter = logging.Formatter(
        '[%(asctime)s] [%(levelname)s] %(name)s (%(funcName)s): %(message)s'
    )

    # Set up console logging
    # TODO: Get logging parameters from cli/config file
    console_handler = logging.StreamHandler()
    console_handler.setLevel((3 - args.verbose) * 10)
    console_handler.setFormatter(default_formatter)
    root_logger.addHandler(console_handler)

    if args.output_file:
        log_handler = logging.StreamHandler(args.output_file)
        try:
            log_level = getattr(logging, args.log_level.upper())
        except AttributeError:
            sys.stderr.write('{0} is not a valid log level'.format(
                args.log_level))
            log_level = logging.DEBUG
        log_handler.setLevel(log_level)
        log_handler.setFormatter(default_formatter)
        root_logger.addHandler(log_handler)

    if args.commtype == 'ethernet':
        conn = communication.EthernetConnection(args.destination, args.port)

    elif args.commtype == 'serial':
        conn = communication.SerialConnection(args.destination, args.speed)

    config = {
        'src_alias': args.src_alias,
    }

    tests = util.get_tests()
    with conn:
        # Run the tests!
        config['dst_alias'], config['dst_id'] = util.discover_node(
            conn, args.src_alias)

        for test in tests:
            if args.test is not None and test.__name__ not in args.test:
                continue

            try:
                test(conn, config)
            except KeyboardInterrupt:
                exit(130)
            except:
                root_logger.exception(
                    'Error running test {0}'.format(
                        test.__name__
                    )
                )
