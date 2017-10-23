OCAML = ocamlbuild
OCAMLFLAGS = -use-ocamlfind

TARGET = BINSEC

BUILDDIR = build
SRCDIR   = src


.PHONY: all build clean

all: build

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

build: $(BUILDDIR)
	$(OCAML) $(OCAMLFLAGS) -I $(SRCDIR) -r -build-dir $(BUILDDIR) main.native
	cp $(BUILDDIR)/$(SRCDIR)/main.native $(BUILDDIR)/Lifter$(TARGET)

clean:
	rm -rf $(BUILDDIR)
