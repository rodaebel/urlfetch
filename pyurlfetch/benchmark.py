from pyurlfetch.urlfetch import URLFetchClient
from urllib2 import urlopen
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

        # Testing Urlfetch Service
        b = runUrlFetch(urls[:i])

        table += zip(a, b)

        sys.stderr.write('.')

    sys.stderr.write('\n')

    print("urllib2;urlfetch")

    for row in table:
        print("%.3f;%.3f" % row)
