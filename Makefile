# {{{ GENERAL SETTINGS

all:
	-rm $(EXECUTABLE)
	make $(EXECUTABLE)

APPDIR=app/
SRCDIR=src/
HSSOURCE=$(find $APPDIR/ $SRCDIR/hs "*.hs")

EXECUTABLE=Relcode.x

# }}}

# HASKELL SETTINGS {{{

STACK_OPTIONS=--copy-bins --local-bin-path=.

# }}}

# COMPILATION {{{

$(EXECUTABLE): $(HSSOURCE)
	stack build $(STACK_OPTIONS)

# }}}

# ADMINISTRATIVE{{{

%/:
	mkdir -p $@

clean:
	rm -r $(BUILDDIR)

# }}}
