import argparse

def main():
    '''Main command-line interface for OpenLCBTests'''

    aparser = argparse.ArgumentParser()
    aparser.add_argument('commtype', choices=['serial', 'enet'],
        help='Specifies the communication method'
    )
    arguments = aparser.parse_args()

    print 'You chose {0}'.format(arguments.commtype)
