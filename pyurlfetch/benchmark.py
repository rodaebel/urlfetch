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
"""Scipt to make benchmarks of pyurlfetch.

Requires Python 2.6 or greater.
"""

from pyurlfetch.urlfetch import URLFetchClient
from urllib2 import urlopen
import multiprocessing
import random
import sys
import time

urls = [
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
    for url in urls:
        ids.append(client.start_fetch(url))
    for Id in ids:
        client.get_result(Id)
    end = time.time()
    results.append(round(end-start, 3))
    client.close()
    return results


if __name__ == "__main__":

    table = []

    for i in range(len(urls)):
        i+=1

        # Shuffle the URLs
        random.shuffle(urls)

        # Testing urllib2
        a = runUrllib2(urls[:i])

        # Testing concurrent urllib2 requests (multiprocessing)
        b = runMultiUrllib2(urls[:i])

        # Testing Urlfetch Service
        c = runUrlFetch(urls[:i])

        table += zip(a, b, c)

        sys.stderr.write('.')

    sys.stderr.write('\n')

    print("urllib2;multiurllib2;urlfetch")

    for row in table:
        print("%.3f;%.3f;%.3f" % row)
