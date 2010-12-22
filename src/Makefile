include ../support/include.mk

PROJECT = urlfetch

ifndef ERL
	ERL := erl
endif

all: $(EBIN_FILES)

edoc:
	$(ERL) -noshell -pa ../ebin/ -eval "edoc:application($(PROJECT), \".\", [{dir, \"../doc/\"}])" -s init stop

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
	rm -f ../doc/*
