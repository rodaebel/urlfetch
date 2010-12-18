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

import BaseHTTPServer
import SimpleHTTPServer
import cgi
import httplib
import logging
import threading
import time
import unittest

LOG_FORMAT = '%(levelname)-8s %(asctime)s %(filename)s:%(lineno)s] %(message)s'


class StoppableHttpRequestHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):
    """HTTP request handler with QUIT stopping the server."""

    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/plain")
        self.send_header("Content-Length", 10)
        self.end_headers()
        self.request.send("pyurlfetch")

    def do_POST(self):
        length = int(self.headers.getheader("Content-Length") or 0)
        data = self.rfile.read(length)
        self.send_response(200)
        self.send_header("Content-Type", "text/plain")
        self.send_header("Content-Length", length)
        self.send_header(
            "X-Custom-Header",
            self.headers.getheader("X-Custom-Header"))
        self.end_headers()
        self.request.send(data)

    def do_QUIT(self):
        """Sends 200 OK response, and sets server.stop to True."""

        self.send_response(200)
        self.end_headers()
        self.server.stop = True

    def log_request(self, *args):
        """Suppress any log messages for testing."""


class StoppableHttpServer(BaseHTTPServer.HTTPServer):
    """HTTP server that reacts to self.stop flag."""

    def serve_forever(self):
        """Handles one request at a time until stopped."""

        self.stop = False
        while not self.stop:
            self.handle_request()


def stop_server(port):
    """Send QUIT request to HTTP server running on localhost:<port>."""

    conn = httplib.HTTPConnection("localhost:%d" % port)
    conn.request("QUIT", "/")
    conn.getresponse()
    conn.close()


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

    def test_headers(self):
        """Encodes/decodes HTTP headers."""

        headers = {'Content-Type': 'text/plain', 'X-Custom-Header': 'foobar'}

        from pyurlfetch.urlfetch import encode_headers, decode_headers

        self.assertEqual(
            'Content-Type: text/plain\nX-Custom-Header: foobar',
            encode_headers(headers))

        self.assertEqual(
            {'Content-Type': 'text/plain', 'X-Custom-Header': 'foobar'},
            decode_headers(encode_headers(headers)))

    def test_error(self):
        """Tests error handling."""

        from pyurlfetch.urlfetch import DownloadError, URLFetchClient

        client = URLFetchClient()

        self.assertRaises(
            DownloadError,
            client.start_fetch, "http://example.com", method="unknown_method")

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

    def test_post(self):
        """POSTing data with the low-level URL Fetch Client API."""

        from pyurlfetch.urlfetch import DownloadError, URLFetchClient

        # Setting up a test HTTP server
        server = StoppableHttpServer(
            ('localhost', 9876), StoppableHttpRequestHandler)
        server_thread = threading.Thread(target=server.serve_forever)
        server_thread.setDaemon(True)
        server_thread.start()

        # Instantiating the client
        client = URLFetchClient()

        # We make a POST request with payload and a custom header
        fid = client.start_fetch(
            "http://localhost:9876",
            payload=u"Très bien".encode("utf-8"),
            method="POST",
            headers={'X-Custom-Header': 'value'})

        code, body, headers = client.get_result(fid)
        self.assertEqual(200, code)
        self.assertEqual(u"Très bien", body.decode("utf-8"))
        self.assertEqual(10, int(headers['content-length']))
        self.assertEqual('value', headers['x-custom-header'])

        # Stop the test HTTP server
        stop_server(9876)

    def test_head(self):
        """Tests a HEAD request."""
       
        from pyurlfetch.urlfetch import DownloadError, URLFetchClient

        client = URLFetchClient()

        fid = client.start_fetch(
            "http://erlang.org/images/erlang-logo.png",
            method="head", headers={"User-Agent": "pyurlfetch/0.1.0"})

        client.close()

        client = URLFetchClient()

        code, body, headers = client.get_result(fid)

        self.assertEqual(200, code)
        self.assertTrue(len(body) is 0)
        self.assertTrue('image/png', headers['content-type'])
