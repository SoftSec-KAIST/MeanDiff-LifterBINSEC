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

all : binsec

# The order of includes is important
# Piqi.mk depends on values set from Config.mk
include Config.mk
include Piqi.mk


CMAKE = cmake
PIN_ROOT_DIR ?= pin-2.14-71313-gcc.4.4.7-linux



BINSEC_DIR = src
binsec:
	$(MAKE) -C $(BINSEC_DIR)

binsec-clean:
	$(MAKE) -C $(BINSEC_DIR) clean

clean:: binsec-clean

clean-configure:
	$(RRM) autom4te.cache config.status configure

veryclean: clean clean-configure


.PHONY: tests
tests:
	$(MAKE) -C tests


include $(BINSEC_DIR)/Targets.mk
