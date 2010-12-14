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
"""Benchmark script for pyurlfetch."""

from pyurlfetch.urlfetch import URLFetchClient
from urllib2 import urlopen
import multiprocessing
import optparse
import random
import sys
import time

USAGE = "usage: %prog [-h] [limit|offset limit]"

URLs = [
    "http://blog.typhoonae.org",
    "http://www.erlang.org",
    "http://www.trapexit.org",
    "http://www.python.org",
    "http://code.google.com",
    "http://www.w3.org",
    "http://dev.mysql.com",
    "http://www.apple.com",
    "http://www.gnustep.org",
    "http://developer.twitter.com",
    "http://code.google.com/p/urlfetch",
    "http://www.debian.com",
    "http://git-scm.com",
    "http://hgbook.red-bean.com",
    "http://www.sgi.com/tech/stl",
]


def runUrllib2(urls):
    """Running benchmark for urllib2.

    Args:
        urls: List of URLs.
    """
    results = []
    start = time.time()
    for url in urls:
        urlopen(url)
    end = time.time()
    results.append(round(end-start, 3))
    return results


def _fetch(url):
    response = urlopen(url).read()


def runMultiUrllib2(urls):
    """Running benchmark for concurrent requests with urllib2.

    Args:
        urls: List of URLs.
    """
    results = []
    start = time.time()
    pool = multiprocessing.Pool(len(urls))
    output = pool.map(_fetch, urls)
    pool.close()
    pool.join()
    end = time.time()
    results.append(round(end-start, 3))
    return results


def runUrlFetch(urls):
    """Running benchmark for the URL Fetch Service.

    Args:
        urls: List of URLs.
    """
    results = []
    client = URLFetchClient()
    ids = []
    start = time.time()
    headers = {'User-Agent': 'urlfetch/1.0'}
    for url in urls:
        ids.append(client.start_fetch(url, headers=headers))
    for Id in ids:
        client.get_result(Id)
    end = time.time()
    results.append(round(end-start, 3))
    client.close()
    return results


if __name__ == "__main__":

    op = optparse.OptionParser(usage=USAGE)

    (options, args) = op.parse_args()

    argc = len(args)
    urlc = len(URLs)

    offset = 1
    limit = urlc

    try:
        if argc == 1:
            limit = int(args[0])
        elif argc == 2:
            offset, limit = (int(args[0]), int(args[1]))
    except ValueError, e:
        op.error(e)

    if offset > urlc:
        op.error("out of range")

    end = offset + limit - 1

    if end > urlc:
        end = urlc

    table = []

    for i in range(offset, end+1):

        # Shuffle the URLs
        urls = URLs[:]
        random.shuffle(urls)

        # Testing urllib2
        a = runUrllib2(urls[:i])

        # Testing concurrent urllib2 requests (multiprocessing)
        b = runMultiUrllib2(urls[:i])

        # Testing the URL Fetch Service (pyurlfetch)
        c = runUrlFetch(urls[:i])

        table += zip(a, b, c)

        sys.stderr.write('.')

    sys.stderr.write('\n')

    print("urllib2;multiurllib2;pyurlfetch")

    for row in table:
        print("%.3f;%.3f;%.3f" % row)
