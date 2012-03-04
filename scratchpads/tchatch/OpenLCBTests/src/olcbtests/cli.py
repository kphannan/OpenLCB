from messages import can
import argparse
import comms

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
    if args.commtype == 'ethernet':
        conn = comms.EthernetConnection(args.hostname, args.port)
        conn.connect()
    #elif arguments.commtype == 'serial':
    #    conn = comms.SerialConnection()
   
    ### Test Implementation ###
    msg = can.VerifyNodeIDNumberSimple(args.src_alias)
    conn.send(msg)
    print msg
    response = conn.receive()
    conn.close()
    print response
