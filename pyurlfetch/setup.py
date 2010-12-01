# Copyright 2010 Tobias Rodaebel
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""Setup script for the pyurlfetch package."""

from distutils.cmd import Command
from setuptools import setup, find_packages
from unittest import TestLoader, TextTestRunner

import os
import sys


class test(Command):
    """Runs the unit tests for pyurlfetch."""

    description = "Runs unit tests for pyurlfetch."

    user_options = [
        ('address=', None, 'address of the urlfetch service')
    ]

    def initialize_options(self):
        self.address = None

    def finalize_options(self):
        pass

    def run(self):
        address = self.address or 'localhost:10190'

        os.environ.setdefault('URLFETCH_ADDR', address)
        import pyurlfetch.tests

        loader = TestLoader()
        t = TextTestRunner()
        t.run(loader.loadTestsFromModule(pyurlfetch.tests))

# 'test' is the parameter as it gets added to setup.py
cmdclasses = {'test': test}


def read(*rnames):
    return open(os.path.join(os.path.dirname(__file__), *rnames)).read()


setup(
    name='pyurlfetch',
    version='0.1.0',
    author="Tobias Rodaebel",
    author_email="tobias.rodaebel@googlemail.com",
    description=("Python client to perform asynchronous URL fetch calls."),
    long_description=(
        read('README.rst')
        + '\n\n' +
        read('CHANGES.rst')
        ),
    license="Apache License 2.0",
    keywords="wget curl url fetch",
    classifiers=[
        'Development Status :: 4 - Beta',
        'Environment :: Web Environment',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: Apache Software License',
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Topic :: Internet :: WWW/HTTP',
        ],
    url='http://code.google.com/p/pyurlfetch',
    packages=find_packages('src'),
    package_dir={'': 'src'},
    include_package_data=True,
    install_requires=[
        'distribute',
    ],
    zip_safe=False,
    cmdclass=cmdclasses
)
