include ../master.Makefile

#MYSITE=/gnu/mysite/mysite
MYSITE=/opt/mysite/mysite
#MYSITE=/local/mysite/mysite
RSS2CONTENT=/opt/godi/bin/cduce -I /opt/mysite --run rss.cdo --arg


#DEST_DIR=site/
#WWW_CVS=/work/devel/all-cameleon2/website/
DEST_DIR=$(ROOT)/web/website

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
	cp top.jpg bottom.jpg $(DEST_DIR)
	(cd $(ROOT)/src && $(MAKE) camlget && $(CP) -r ocamldoc $(DEST_DIR)/)
	$(CP) $(DEST_DIR)/index.en.html $(DEST_DIR)/index.html
	rpl "&amp;&amp;" "&&" $(DEST_DIR)/*.html

installsite:site
#	$(CP) -r $(DEST_DIR)* $(WWW_CVS)
#	$(CP) $(DEST_DIR)index.en.html $(WWW_CVS)index.html
#	(cd $(WWW_CVS) && touch img/* && svn commit -m-)
#	$(CP) /work/devel/www-cameleon2/* ~/public_html/tmp_cameleon2/
clean:
	$(RM) *~ \#*\#
