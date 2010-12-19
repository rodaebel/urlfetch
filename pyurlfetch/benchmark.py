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

USAGE = "usage: %prog [-chn] URL [...]"


def runUrllib2(urls, num):
    """Running benchmark for urllib2.

    Args:
        urls: List of URLs.
        num: Number of requests.
    """
    results = []
    for i in range(num):
        sys.stderr.write('.')
        start = time.time()
        for url in urls:
            urlopen(url)
        end = time.time()
        results.append(round(end-start, 3))
    return results


def _fetch(url):
    response = urlopen(url).read()


def runMultiUrllib2(urls, num):
    """Running benchmark for concurrent requests with urllib2.

    Args:
        urls: List of URLs.
        num: Number of requests.
    """
    results = []
    for i in range(num):
        sys.stderr.write('.')
        start = time.time()
        pool = multiprocessing.Pool(len(urls))
        output = pool.map(_fetch, urls)
        pool.close()
        pool.join()
        end = time.time()
        results.append(round(end-start, 3))
    return results


def runUrlFetch(urls, num):
    """Running benchmark for the URL Fetch service.

    Args:
        urls: List of URLs.
        num: Number of requests.
    """
    results = []
    headers = {'User-Agent': 'urlfetch/1.0'}
    client = URLFetchClient()
    for i in range(num):
        sys.stderr.write('.')
        ids = []
        start = time.time()
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

    op.add_option("-c", dest="concurrent", metavar="INTEGER",
                  help="number of concurrent requests (default: %default)",
                  default=10)

    op.add_option("-n", dest="num_requests", metavar="INTEGER",
                  help="number of requests (default: %default)",
                  default=1)

    op.add_option("--skip-synchronous", dest="skip_sync", action="store_true",
                  help="skip synchronous urllib2.urlopen calls",
                  default=False)

    (options, args) = op.parse_args()

    if not args:
        op.error("at least one URL required")

    c = int(options.concurrent)
    n = int(options.num_requests)

    urls = args

    step = c / len(urls)

    index = 0

    bucket = []

    for i in range(c):

        if float(i)/float(step) == index and index < len(urls):
            index += 1

        bucket.append(urls[index-1])

    if not options.skip_sync:
        # Testing synchronous urllib2 requests
        A = runUrllib2(bucket, n)

    # Testing concurrent urllib2 requests (multiprocessing)
    B = runMultiUrllib2(bucket, n)

    # Testing the URL Fetch service (pyurlfetch)
    C = runUrlFetch(bucket, n)

    sys.stderr.write('\n')

    if options.skip_sync:
        table = zip(B, C)
        averages = (sum(B)/n, sum(C)/n)
        f = "%.3f;%.3f"
    else:
        table = zip(A, B, C)
        averages = (sum(A)/n, sum(B)/n, sum(C)/n)
        f = "%.3f;%.3f;%.3f"

    for row in table:
        print(f % row)
    print('---')
    print(f % averages)
