#################################################################################
#                Hypomnemata                                                    #
#                                                                               #
#    Copyright (C) 2010 Institut National de Recherche en Informatique          #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Library General Public License as            #
#    published by the Free Software Foundation; either version 2 of the         #
#    License, or any later version.                                             #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU Library General Public          #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#                                                                               #
#################################################################################


include master.Makefile

SCRIPTS=

IMAGES=

# Compilation
#############

work: src

all: work srcdoc doc

src: dummy
	cd src && $(MAKE) all

re : depend clean all


# Documentation :
#################
srcdoc: dummy
	cd src && $(MAKE) doc

doc: dummy
	cd doc && $(MAKE) all

# myself

master.Makefile src/version.ml src/install.ml: \
	master.Makefile.in src/version.ml.in src/install.ml.in config.status
	./config.status

config.status: configure master.Makefile.in src/version.ml.in src/install.ml.in
	./config.status --recheck

configure: configure.ac
	autoconf

# headers :
###########
HEADFILES= configure.ac configure \
	master.Makefile.in Makefile \
	src/*/*.ml src/*/*.mli src/*/*.mll src/*/*.mly src/*/*.in \
	src/gtk/*.ml src/gtk/*.mli \
	src/Makefile doc/Makefile checkocaml.ml
headers: dummy
	echo $(HEADFILES)
	headache -h header -c .headache_config `ls $(HEADFILES) `

noheaders: dummy
	headache -r -c .headache_config `ls $(HEADFILES) `

# backup, clean and depend :
############################

distclean: clean
	cd src && $(MAKE) distclean
	cd doc && $(MAKE) distclean
	cd utils && $(MAKE) distclean
	$(RM) config.cache config.log config.status master.Makefile
	$(RM) config_check.log ocaml_config.sh

clean: dummy
	$(RM) *~ \#*\#
	cd src && $(MAKE) clean
	cd doc && $(MAKE) clean
	cd utils && $(MAKE) clean

depend: dummy
	cd src && $(MAKE) depend
alldepend: dummy
	cd src && $(MAKE) alldepend

dummy:

#################
# installation
#################

install: dummy installimages
	cd src && $(MAKE) install
#	$(MAKE) installspecsfiles
	$(MAKE) installdoc

###########################
# additional dependencies
###########################

# DO NOT DELETE
