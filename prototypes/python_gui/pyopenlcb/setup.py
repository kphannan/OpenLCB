import sys
try:
    from setuptools import setup, find_packages
except ImportError:
    import distribute_setup
    distribute_setup.use_setuptools()
    from setuptools import setup, find_packages

install_requires=[
    'setuptools',
    'PySerial',
]

if sys.version_info < (2, 6):
    raise Exception('Python 2.6 or later is required')

setup(
    name='PyOpenLCB',
    version='0.3',
    description='''\
Implementation of the OpenLCB model train control protocol in Python''',
    author='Dustin C. Hatch',
    author_email='admiralnemo@gmail.com',
    url='https://bitbucket.org/AdmiralNemo/pyopenlcb',
    license='APACHE-2.0',
    classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: Apache Software License',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.1',
        'Programming Language :: Python :: 3.2',
        'Programming Language :: Python :: Implementation :: CPython',
        'Topic :: Software Development :: Libraries :: Python Modules',
        'Topic :: System :: Hardware',
        'Topic :: System :: Networking'
    ],
    install_requires=install_requires,
    packages=find_packages('src'),
    package_dir={'': 'src'},
    tests_require=['nose'],
    test_suite='nose.collector',
)
