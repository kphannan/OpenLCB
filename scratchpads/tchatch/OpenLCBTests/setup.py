import sys
try:
    from setuptools import setup, find_packages
except ImportError:
    import distribute_setup
    distribute_setup.use_setuptools()
    from setuptools import setup, find_packages

install_requires=[
    'setuptools',
]

if sys.version_info < (2, 7):
    install_requires.append('ArgParse')

setup(
    name='OpenLCBTests',
    version='0.1',
    description='',
    author='Timothy C. Hatch',
    author_email='tim@openlcb.net',
    url='',
    license='APACHE-2.0',
    classifiers=[
    ],
    install_requires=install_requires,
    packages=find_packages('src'),
    package_dir={'': 'src'},
    entry_points={
        'console_scripts': [
            'olcbtest = olcbtests.cli:main',
        ],
    }
)

