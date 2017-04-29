##########################################################################
#  This file is part of Binsec.                                          #
#                                                                        #
#  Copyright (C) 2016-2017                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

CAMLC		?=ocamlc.opt
CAMLOPT		?=ocamlopt.opt
CAMLLEX		?=ocamllex
CAMLLEXOPTS?	?=
CAMLYAC		?=menhir
CAMLYACOPTS	?=
CAMLDEP		?=ocamldep
CAMLWARNINGS?	?=-w +a-4-3
CAMLFLAGS?	?=-g -annot
CAMLDOC		?=ocamldoc
CAMLFIND	?=ocamlfind
BEST		?=opt

OCAMLBUILD?=ocamlbuild
OCB_OPTIONS?=-use-ocamlfind -no-hygiene -libs str,bigarray -I .

PIQI ?=piqi
PIQI_FLAGS ?= of-proto -I $(PROTO_DIR)

PROTOC ?=protoc
PROTOC_FLAGS ?= --proto_path=$(BINSEC_DIR)/$(PROTO_DIR) --cpp_out=$(CPP_PROTOBUF_DIR)

PIQI2CAML ?=piqic-ocaml
PIQI2CAML_FLAGS ?= --multi-format -C $(PIQI_DIR) 

DESTDIR         ?=
prefix          ?=/usr/local
exec_prefix     ?=${prefix}
datarootdir     ?=${prefix}/share
datadir         ?=${datarootdir}
BINDIR          ?="$(DESTDIR)${exec_prefix}/bin"
LIBDIR          ?="$(DESTDIR)${exec_prefix}/lib"
DATADIR         ?="$(DESTDIR)${prefix}/share"
MANDIR          ?="$(DESTDIR)${datarootdir}/man"

VERBOSEMAKE?=no

ifneq ($(VERBOSEMAKE),no) # Do not change to ifeq ($(VERBOSEMAKE),yes), as this
			  # version makes it easier for the user to set the
			  # option on the command-line to investigate
			  # Makefile-related problems
# ignore the PRINT_* materials but print all the other commands
  PP = @true
# prevent the warning "jobserver unavailable: using -j1".
# see GNU make manual (section 5.7.1 and appendix B)
  QUIET_MAKE:= + $(MAKE)
# prevent the warning: "-jN forced in submake: disabling jobserver mode".
# see GNU make manual (appendix B)
  MAKE := MAKEFLAGS="$(patsubst j,,$(MAKEFLAGS))" $(MAKE)
else
# print the PP_* materials
  PP = @echo
# but silently execute all the other commands
# fixed bug #637: do not write spaces between flags
  OLDFLAGS:=r$(MAKEFLAGS)
  MAKEFLAGS:=rs$(MAKEFLAGS)
# do not silently execute other makefiles (e.g the one of why):
# the redefinition of MAKE below is for this purpose
# but use QUIET_MAKE in order to call silently the initial Makefile
  QUIET_MAKE:= $(MAKE)
  MAKE := MAKEFLAGS="$(OLDFLAGS)" $(MAKE)
endif

PP_BYT   = $(PP) 'BYT  '
PP_OPT   = $(PP) 'BIN  '
PP_YACC  = $(PP) 'MENHIR '
PP_LEX   = $(PP) 'LEX  '

RM ?= rm -rf
RRM ?= rm -rf
CP ?= cp -r
MKDIR ?= mkdir -p
CD ?= cd
TAR ?= tar cvzf
INSTALL ?= install

HEADACHE=no
HEADACHE_CONFIG?=headers/headache_config.txt

DOT2SVG = dot -Tsvg

%.svg : %.dot
	$(DOT2SVG) -o $@ $<
