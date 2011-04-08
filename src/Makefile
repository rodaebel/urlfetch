include ../support/include.mk

PROJECT = urlfetch

ifndef ERL
	ERL := erl
endif

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
