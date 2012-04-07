#################################################################################
#                Lablgtk-extras                                                 #
#                                                                               #
#    Copyright (C) 2011 Institut National de Recherche en Informatique          #
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

# check for one particular file of the sources
AC_INIT(master.Makefile.in)

VERSION=1.1

# The root directory where we will compile
ROOT=`pwd`

# Check for Ocaml compilers

WINDOWS=0
AC_ARG_ENABLE(windows,
[  to use when compiling under windows/cygwin
],
[ WINDOWS=1])

if test ${WINDOWS} = 1 ; then
  INCS='-I "C:\ocamlmgw\lib"  -I "C:\ocamlmgw\lib\lablgtk2" -ccopt "-LC:/opt/gtk/lib"';
  echo '
OCAMLDEP="ocamldep.opt"
OCAMLYACC="ocamlyacc"
OCAMLDOC="ocamldoc"
OCAMLLIB="/cygdrive/c/ocamlmgw/lib"
OCAMLDOC_OPT="ocamldoc.opt"
OCAMLMKTOP="ocamlmktop"
OCAMLPROF="ocamlprof"
OCAMLBIN="/cygdrive/c/ocamlmgw/bin"
OCAMLC="ocamlc.opt"
OCAMLDOC_PLUGINSDIR="/cygdrive/c/ocamlmgw/lib/ocamldoc/custom"
OCAMLFIND="ocamlfind"
CAMLP4="camlp4"
OCAMLLEX="ocamllex.opt"
OCAML="ocaml"
OCAMLOPT="ocamlopt.opt"
OCAMLMKLIB="ocamlmklib"
MENHIR="menhir"
' > ./ocaml_config.sh
else
  ocaml unix.cma str.cma checkocaml.ml ocaml_config.sh || exit 1
fi
rm -f foo.os foo.os2
. ./ocaml_config.sh

# Windows shit
if test ${WINDOWS} = 1 ; then
  OCAMLDOC="$OCAMLDOC $INCS";
  OCAMLDOC_OPT="$OCAMLDOC_OPT $INCS";
  OCAMLMKTOP="$OCAMLMKTOP $INCS";
  OCAMLC="$OCAMLC $INCS";
  OCAML="$OCAML $INCS";
  OCAMLOPT="$OCAMLOPT $INCS";
  OCAMLMKLIB="$OCAMLMKLIB $INCS";
fi

echo "ocaml library path is $OCAMLLIB"

# set the default prefix to the ocaml directory
AC_PREFIX_PROGRAM(`basename $OCAML`)
exec_prefix=$prefix



############################
# Flags
############################
COMPFLAGS=

################
# tool and lib names
################
GELIB=lablgtkextras.cmxa
GELIB_BYTE=lablgtkextras.cma

CWLIB=configwin.cmxa
CWLIB_BYTE=configwin.cma

#############################
# substitutions to perform

AC_SUBST(VERSION)

AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLMKTOP)
AC_SUBST(OCAMLMKLIB)
AC_SUBST(CAMMKTOP)
AC_SUBST(OCAMLBIN)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLDOC_OPT)
AC_SUBST(EXEC_PREFIX)
AC_SUBST(INSTALLBIN)
AC_SUBST(ROOT)
AC_SUBST(OCAMLBUILD)
AC_SUBST(OCAMLFIND)

AC_SUBST(GELIB)
AC_SUBST(GELIB_BYTE)
AC_SUBST(CWLIB)
AC_SUBST(CWLIB_BYTE)

AC_SUBST(XMLLIGHT_INCLUDES)

# Finally create the master.Makefile.in and other files
ACFILES="master.Makefile src/version.ml src/install.ml"

AC_OUTPUT($ACFILES)
chmod a-w master.Makefile
chmod a-w src/version.ml src/install.ml master.Makefile

# List values
echo "
### Results of configuration ###

Libraries will be installed with findlib (package lablgtk-extras)

"
