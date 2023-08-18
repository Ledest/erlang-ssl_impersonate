ERL ?= erl
ERLC ?= $(ERL)c
ERLC_OPTS = +debug_info

SSL_DIR != $(ERL) -noshell -eval 'io:put_chars(code:lib_dir(ssl)), init:stop()'
SSL_DIR_SRC = $(SSL_DIR)/src
DIFFS_ERL = $(wildcard priv/patches/*.erl.diff)
DIFFS = $(wildcard priv/patches/*.diff)
SOURCES = $(DIFFS_ERL:priv/patches/%.erl.diff=priv/ssl/src/%.erl)
FILES = $(DIFFS:priv/patches/%.diff=priv/ssl/src/%)
SSL_FILES = $(subst priv/ssl/src,$(SSL_DIR_SRC),$(FILES))
MODULES = $(SOURCES:priv/ssl/src/%.erl=priv/ssl/ebin/%.beam)

$(FILES): $(DIFFS) $(SSL_FILES) priv/ssl/src/
	for i in $(DIFFS); do \
	    n=$$(basename $$i .diff); \
	    patch -p0 -i $$i -o priv/ssl/src/$$n $(SSL_DIR_SRC)/$$n; \
	done

$(MODULES): $(FILES) priv/ssl/ebin/
	$(ERLC) -o priv/ssl/ebin -I $(SSL_DIR_SRC) $(ERLC_OPTS) $(SOURCES)

priv/ssl/src/:
	install -d -m 0766 priv/ssl/src

priv/ssl/ebin/:
	install -d -m 0766 priv/ssl/ebin

compile: $(MODULES)

clean:
	rm -rf priv/ssl
