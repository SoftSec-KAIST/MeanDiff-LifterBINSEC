OCAML = ocamlbuild
OCAMLFLAGS = -use-ocamlfind

TARGET = bincoa-trans

BUILDDIR = build
SRCDIR = src

SRC = $(wildcard $(SRDDIR)/**/*.mli) $(wildcard $(SRDDIR)/**/*.ml)


.PHONY: all build clean
all: build

$(BUILDDIR)/$(SRCDIR)/main.byte: $(SRC)
	$(OCAML) $(OCAMLFLAGS) -I $(SRCDIR) -build-dir $(BUILDDIR) main.byte

$(BUILDDIR)/$(TARGET): $(BUILDDIR)/$(SRCDIR)/main.byte
	cp $< $@

build: $(BUILDDIR)/$(TARGET)

clean:
	rm -rf $(BUILDDIR)
