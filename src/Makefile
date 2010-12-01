include ../support/include.mk

PROJECT = urlfetch

all: $(EBIN_FILES)

edoc:
	erl -noshell -pa ../ebin/ -eval "edoc:application($(PROJECT), \".\", [{dir, \"../doc/\"}])" -s init stop

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
	rm -f ../doc/*
