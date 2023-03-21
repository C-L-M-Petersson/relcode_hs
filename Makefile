# {{{ GENERAL SETTINGS

all:
	-rm $(EXECUTABLE)
	make $(EXECUTABLE)

SRCDIR=src

BUILDDIR=localbuild
HIDIR=$(BUILDDIR)/hi/
ODIR=$(BUILDDIR)/o/
EXECUTABLE=run.x

MAIN=$(SRCDIR)/Main.hs

# }}}

# HASKELL SETTINGS {{{

GHC=ghc --make
GHCDIRS=-odir $(ODIR) -hidir $(HIDIR) -i$(SRCDIR)
GHCOPTS=-dynamic -O2 $(patsubst %, -package %, $(GHCPKGS))
GHCPKGS=base           \
        composition    \
        extra          \
        gamma          \
        lens           \
        mtl            \
        safe           \
        split          \
        utility-ht     \
        wigner-symbols \
        yjtools

# }}}

# CPP SETTINGS {{{

CPPINCLUDE=-I/usr/include/lammps/
CPPLIBS=-llammps -lmpi -lopen-rte -lopen-pal -L/usr/lib/openmpi

# }}}

# COMPILATION {{{

$(EXECUTABLE): $(MAIN) $(ODIR) $(HIDIR)
	$(GHC) -o $@ $(GHCDIRS) $(GHCOPTS) $(CPPINCLUDE) $(CPPLIBS) $<

# }}}

# ADMINISTRATIVE{{{

%/:
	mkdir -p $@

clean:
	rm -r $(BUILDDIR)

# }}}
