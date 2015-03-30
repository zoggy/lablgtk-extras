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

include ../master.Makefile

PACKAGES=config-file,lablgtk2.sourceview2,xmlm
OF_FLAGS= -package $(PACKAGES)

COMPFLAGS=-annot -g -warn-error +1..49-3

GELIB_CMOFILES= \
	gtke_version.cmo \
	gtke_install.cmo \
	gstuff.cmo \
	okey.cmo \
	gdir.cmo \
	gmylist.cmo \
	gmytree.cmo \
	gtksv_utils.cmo

GELIB_CMXFILES=$(GELIB_CMOFILES:.cmo=.cmx)
GELIB_CMIFILES=$(GELIB_CMOFILES:.cmo=.cmi)

CWLIB_CMOFILES=configwin_keys.cmo \
        configwin_types.cmo \
        configwin_messages.cmo \
        configwin_ihm.cmo \
        configwin.cmo
CWLIB_CMIFILES=$(CWLIB_CMOFILES:.cmo=.cmi)
CWLIB_CMXFILES=$(CWLIB_CMOFILES:.cmo=.cmx)

all: byte opt
opt: $(GELIB) $(CWLIB)
byte:  $(GELIB_BYTE) $(CWLIB_BYTE)

TOOLS=
TOOLS_BYTE=

# lablgtkextras
$(GELIB): $(GELIB_CMIFILES) $(GELIB_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(GELIB_CMXFILES)

$(GELIB_BYTE): $(GELIB_CMIFILES) $(GELIB_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(GELIB_CMOFILES)

# configwin
$(CWLIB): $(CWLIB_CMIFILES) $(CWLIB_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(CWLIB_CMXFILES)

$(CWLIB_BYTE): $(CWLIB_CMIFILES) $(CWLIB_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(CWLIB_CMOFILES)

#client: $(GELIB) client.ml
#	$(OCAMLOPT) -o $@ $(INCLUDES) lablgtk.cmxa lablgnomecanvas.cmxa $^

doc:
	$(RM) ocamldoc
	mkdir -p ocamldoc
	$(OCAMLFIND) ocamldoc -verbose $(OF_FLAGS) -sort -d ocamldoc -html  -t "Lablgtk-extras" \
	$(INCLUDES) $(GELIB_CMIFILES:.cmi=.mli) \
	$(CWLIB:.cmxa=.mli) configwin_keys.ml
#	$(CP) ocamldoc_style.css ocamldoc/style.css

camlget:
	$(RM) ocamldoc
	mkdir -p ocamldoc
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) -sort -d ocamldoc -t "Lablgtk-extras" \
	-g odoc_htmlcg.cmo $(INCLUDES) \
	$(GELIB_CMIFILES:.cmi=.mli) $(CWLIB:.cmxa=.mli) configwin_keys.ml
	$(CP) camlget.png ocamldoc/
#	$(CP) ocamldoc_style.css ocamldoc/style.css

dummy:

install:
	@$(OCAMLFIND) install $(PACKAGE_NAME) META \
	  $(GELIB) $(GELIB_BYTE) $(GELIB:.cmxa=.a) $(GELIB_CMIFILES) \
	  $(GELIB_CMOFILES) $(GELIB_CMXFILES) \
	  $(GELIB_CMXFILES:.cmx=.o) $(GELIB_CMIFILES:.cmi=.mli) \
	  $(CWLIB) $(CWLIB_BYTE) $(CWLIB:.cmxa=.a) $(CWLIB:.cmxa=.cmi) $(CWLIB:.cmxa=.mli)

uninstall:
	@$(OCAMLFIND) remove $(PACKAGE_NAME)

# Clean :
#########
clean:
	$(RM) $(GENERATED_FILES) *.cm* *.so *.a *.o *.annot

distclean: clean

# depend :
##########
GENERATED_FILES=
.depend depend: $(GENERATED_FILES)
	$(RM) .depend
	$(OCAMLDEP) $(OCAMLPP) *.ml *.mli > .depend

