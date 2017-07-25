OCAML = ocamlbuild
OCAMLFLAGS = -use-ocamlfind

TARGET = BINSEC

BUILDDIR = build
SRCDIR   = src


.PHONY: all build clean

all: build

build:
	$(OCAML) $(OCAMLFLAGS) -I $(SRCDIR) -r -build-dir $(BUILDDIR) main.native
	cp $(BUILDDIR)/$(SRCDIR)/main.native $(BUILDDIR)/$(TARGET)

clean:
	rm -rf $(BUILDDIR)
