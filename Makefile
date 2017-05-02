OCAML = ocamlbuild
OCAMLFLAGS = -use-ocamlfind

TARGET = bincoa

BUILDDIR = build
SRCDIR   = src


.PHONY: all build clean

all: build

build:
	$(OCAML) $(OCAMLFLAGS) -I $(SRCDIR) -r -build-dir $(BUILDDIR) $(TARGET).byte
	cp $(BUILDDIR)/$(SRCDIR)/$(TARGET).byte $(BUILDDIR)/$(TARGET)

clean:
	rm -rf $(BUILDDIR)
