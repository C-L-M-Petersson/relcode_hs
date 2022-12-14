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
GHCPKGS=base        \
        composition \
        extra       \
        mtl         \
        safe        \
        split       \
        utility-ht
        #bytestring            \
        #composition           \
        #directory             \
        #extra                 \
        #haskell-mpi           \
        #mtl                   \
        #numeric-limits        \
        #random                \
        #silently              \
        #system-posix-redirect \
        #unix                  \

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
