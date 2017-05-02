OCAML = ocamlbuild
OCAMLFLAGS = -use-ocamlfind

TARGET = bincoa-trans

BUILDDIR = build
SRCDIR = src


.PHONY: all build clean

all: build

build:
	$(OCAML) $(OCAMLFLAGS) -I $(SRCDIR) -build-dir $(BUILDDIR) main.byte
	cp $(BUILDDIR)/$(SRCDIR)/main.byte $(BUILDDIR)/$(TARGET)

clean:
	rm -rf $(BUILDDIR)
