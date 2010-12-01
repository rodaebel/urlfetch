from pyurlfetch.urlfetch import URLFetchClient
from urllib2 import urlopen
import random
import time

urls = [
    "http://blog.typhoonae.org",
    "http://www.erlang.org",
    "http://www.trapexit.org",
    "http://www.python.org",
    "http://code.google.com",
    "http://www.w3.org",
    "http://dev.mysql.com",
    "http://www.debian.org",
    "http://www.gnustep.org",
    "http://developer.twitter.com",
]


def runUrllib2(urls):
    """Running benchmark for urllib2.

    Args:
        urls: List of URLs.
    """
    start = time.time()
    for url in urls:
        urlopen(url)
    end = time.time()
    print "urllib2", "\t", round(end-start, 3), 's'


def runUrlFetch(urls):
    """Running benchmark for the URL Fetch Service.

    Args:
        urls: List of URLs.
    """
    client = URLFetchClient()
    ids = []
    start = time.time()
    for url in urls:
        ids.append(client.start_fetch(url))

    for Id in ids:
        client.get_result(Id)
    end = time.time()
    print "urlfetch", "\t", round(end-start, 3), 's'
    client.close()


if __name__ == "__main__":

    for i in range(len(urls)):
        i+=1

        # Shuffle the URLs
        random.shuffle(urls)

        print "fetching %i URL(s):" % i

        # Testing urllib2
        runUrllib2(urls[:i])

        # Testing Urlfetch Service
        runUrlFetch(urls[:i])
