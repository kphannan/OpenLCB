from olcbtests.messages import can
import imp
import logging
import pkg_resources
import sys

logger = logging.getLogger(__name__)

def is_nodetest(obj):
    '''Indicate whether an object is a node conformance test

    Node conformance tests are callables that meet one or more of the
    following criteria:

    * Name begins with *nodetest* (case insensitive)
    * Has an attribute ``is_nodetest`` that evaluates to a true value
    * Inherits from any class whose name begins with *nodetest* (case
      insensitive

    Node conformance tests must also have the following signature::

        def func(conn, config)

    * ``conn`` is an instance of
       :py:class:`~olcbtests.comms.EthernetConnection`
    * ``config`` is a mapping of configuration settings that have been
       obtained by the test framework (i.e. from the user or a config
       file) that can be used by the node conformance test

    The easiest way to mark a function as a test is to use the
    :py:func:`nodetest` decorator::

        from olcbtests.tests.util import nodetest
        
        @nodetest
        def do_nothing(conn, config):
            pass
    '''

    if not hasattr(obj, '__call__') or type(obj) is type:
        return False

    if (getattr(obj, '__name__', '').lower().startswith('nodetest') and
        obj is not nodetest):
        return True
    if getattr(obj, 'is_nodetest', False):
        return True
    if obj.__class__.__name__.lower().startswith('nodetest'):
        return True
    return True in (
        b.__name__.lower().startswith('nodetest')
        for b in obj.__class__.__bases__
        )


def nodetest(func):
    '''Mark an object as a node conformance test'''

    func.is_nodetest = True
    return func


def get_tests():
    '''Find and return all node conformance tests provided by OpenLCBTests'''

    contents = pkg_resources.resource_listdir(__package__, None)
    mod_suffixes = [s.rsplit('.', 1)[-1] for s, _, _ in imp.get_suffixes()]
    modules = set()
    for item in contents:
        if not pkg_resources.resource_isdir(__package__, item):
            try:
                name, ext = item.rsplit('.', 1)
            except ValueError:
                continue
            if ext not in mod_suffixes:
                logger.debug(
                    'Resource {item} in {package} is not a module'.format(
                        item=item,
                        package=__package__
                    )
                )
                continue
            logger.debug('Resource {item} in {package} is a module'.format(
                item=item,
                package=__package__
                )
            )
            modules.add(name)

    tests = set()
    for name in modules:
        try:
            mod_name = '.'.join((__package__, name))
            __import__(mod_name)
            test_mod = sys.modules[mod_name]
        except ImportError:
            continue
        else:
            logger.debug('Loaded module {0}'.format(mod_name))

        for name in dir(test_mod):
            obj = getattr(test_mod, name)
            if is_nodetest(obj):
                tests.add(obj)

    logger.info(
        'Found tests: {0}'.format(
            ', '.join(t.__name__ for t in tests)
        )
    )
    tests = list(tests)
    tests.sort()
    return tests


def discover_node(conn, src_alias):
    '''Discover the attached node using a Verify Node ID Number message
    
    :returns tuple: A 2-tuple containing the node alias and node ID, in
       that order
    '''

    msg = can.VerifyNodeIDNumberSimple(src_alias=src_alias)
    conn.send(msg)
    r = conn.receive()
    response = can.VerifiedNodeIDNumber.from_string(r)

    logger.info('Discovered node {alias} ({id})'.format(
        alias=response.src_alias,
        id=response.node_id
    ))

    return response.src_alias, response.node_id

