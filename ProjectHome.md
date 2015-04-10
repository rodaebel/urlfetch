The urlfetch project aims at providing a URL Fetch Service for web application developers to make concurrent asynchronous HTTP/S requests. The service is entirely written in [Erlang](http://www.erlang.org) and so, it leverages the robustness and scalability of the Erlang _Open Telecom Platform_.

Pyurlfetch is a tiny Python urlfetch client library. The chart below shows a [benchmark](http://code.google.com/p/urlfetch/source/browse/pyurlfetch/benchmark.py) of concurrent requests using [urllib2](http://docs.python.org/library/urllib2.html) with [multiprocessing](http://docs.python.org/library/multiprocessing.html) vs. concurrent asynchronous requests using [pyurlfetch](http://code.google.com/p/urlfetch/source/browse/#hg/pyurlfetch).

![http://wiki.urlfetch.googlecode.com/hg/urlfetch.png](http://wiki.urlfetch.googlecode.com/hg/urlfetch.png)

Another client library for the [Ruby](http://ruby-lang.org) programming language is available [here](http://github.com/rodaebel/urlfetch).