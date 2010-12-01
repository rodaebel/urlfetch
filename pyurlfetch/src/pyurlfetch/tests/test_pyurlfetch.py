# -*- coding: utf-8 -*-
#
# Copyright 2010 Tobias Rodäbel
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
"""Unit tests for the pyurlfetch library."""

import logging
import time
import unittest

LOG_FORMAT = '%(levelname)-8s %(asctime)s %(filename)s:%(lineno)s] %(message)s'


class TestUrlFetch(unittest.TestCase):
    """Test case for pyurlfetch."""

    def setUp(self):
        """Setup test environment."""

        logging.basicConfig(format=LOG_FORMAT, level=logging.INFO)

    def test_import(self):
        """Importing the pyurlfetch module."""

        import pyurlfetch
        self.assertEqual('module', type(pyurlfetch).__name__)

    def test_pack(self):
        """Packing data."""

        from pyurlfetch.urlfetch import pack

        self.assertEqual('\x03foo', pack(3, 'foo'))

    def test_command(self):
        """Testing the command method."""

        from pyurlfetch.urlfetch import command

        self.assertEqual('foo', command('foo'))
        self.assertEqual('\x03footest', command('foo', 'test'))
        self.assertEqual('\x03foo\x04testbar', command('foo', 'test', 'bar'))

    def test_fetch(self):
        """Testing the low-level URL Fetch Client API."""

        from pyurlfetch.urlfetch import DownloadError, URLFetchClient

        # Instantiating the client
        client = URLFetchClient()

        # Initiating fetches
        fetches = (
            client.start_fetch("http://www.erlang.org"),
            client.start_fetch("http://localhost:1635"), # Shouldn't exist
            client.start_fetch("http://www.python.org"),
        )

        # Doing some heavy stuff
        time.sleep(2)

        # Getting the results
        data = []

        for fid in fetches:
            try:
                data.append(client.get_result(fid))
            except DownloadError, e:
                data.append(e)

        client.close()

        self.assertEqual(2, [isinstance(s, tuple) for s in data].count(True))

    def test_fetch_nowait(self):
        """Testing the low-level URL Fetch Client API."""

        from pyurlfetch.urlfetch import DownloadError, URLFetchClient

        # Instantiating the client
        client = URLFetchClient()

        # Initiating fetches
        fetches = (
            client.start_fetch("http://www.erlang.org"),
            client.start_fetch("http://localhost:1635"), # Shouldn't exist
            client.start_fetch("http://www.python.org"),
        )

        # Doing some heavy stuff
        time.sleep(2)

        # Getting the results
        data = []

        for fid in fetches:
            try:
                data.append(client.get_result(fid, nowait=True))
            except DownloadError, e:
                data.append(e)

        client.close()

        self.assertEqual(3, len(data))
