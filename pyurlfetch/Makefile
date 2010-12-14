ifndef $(PYTHON)
	PYTHON= $(shell which python)
endif

TESTS= $(shell find src -name [a-z]\*.py ! -name \test_*.py)


all: bin/python

bin/buildout: buildout.cfg
	$(PYTHON) bootstrap.py --distribute

bin/python: bin/buildout
	bin/buildout -NU

bin/coverage: bin/python

test: bin/python
	bin/python setup.py test

coverage: bin/coverage
	bin/coverage erase
	bin/coverage run setup.py test
	bin/coverage combine
	bin/coverage html $(TESTS)
	bin/coverage report -m $(TESTS)

distclean:
	rm -rf .coverage .installed.cfg bin/ develop-eggs/ eggs/ htmlcov/ parts/
	for f in `find . -name "*.pyc"`; do (rm -f $$f); done
	for f in `find . -name "*.egg-info"`; do (rm -rf $$f); done
