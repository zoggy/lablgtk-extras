include ../master.Makefile

#MYSITE=/gnu/mysite/mysite
MYSITE=/opt/mysite/mysite
#MYSITE=/local/mysite/mysite
RSS2CONTENT=/opt/godi/bin/cduce -I /opt/mysite --run rss.cdo --arg


#DEST_DIR=site/
#WWW_CVS=/work/devel/all-cameleon2/website/
DEST_DIR=$(ROOT)/web/website
REMOTE_DEST=zoggy@gtk-extras.forge.ocamlcore.org:/home/groups/gtk-extras/htdocs/

XML_FILES=\
	about.xml \
	configwin.xml \
	download.xml \
	okey.xml \
	site.xml \
	sitemap.xml

all: $(XML_FILES)
	$(MKDIR) $(DEST_DIR)
	$(MYSITE) site.xml -lang fr -o $(DEST_DIR)
	$(MYSITE) site.xml -lang en -o $(DEST_DIR)
	cp style.css $(DEST_DIR)
	(cd $(ROOT)/src && $(MAKE) camlget && $(CP) -r ocamldoc $(DEST_DIR)/)
	$(CP) $(DEST_DIR)/index.en.html $(DEST_DIR)/index.html
	$(CP) -r img $(DEST_DIR)
	$(MKDIR) $(DEST_DIR)/refdoc
	$(CP) ../src/ocamldoc/* $(DEST_DIR)/refdoc/
	rpl "&amp;&amp;" "&&" $(DEST_DIR)/*.html

installsite:all
	scp -r $(DEST_DIR)/* $(REMOTE_DEST)
#	$(CP) $(DEST_DIR)index.en.html $(WWW_CVS)index.html
#	(cd $(WWW_CVS) && touch img/* && svn commit -m-)
#	$(CP) /work/devel/www-cameleon2/* ~/public_html/tmp_cameleon2/
clean:
	$(RM) *~ \#*\#
