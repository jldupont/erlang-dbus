#
# @author Jean-Lou Dupont
# @title  Erlang-DBus
#
VERSION=`cat VERSION`
PRJ=erlang_dbus
DEBIAN_PKG_VER=1

DPREFIX=/tmp/$(PRJ)/debian/DEBIAN
CTLFILES=$(DPREFIX)/control $(DPREFIX)/postinst $(DPREFIX)/postrm $(DPREFIX)/preinst $(DPREFIX)/prerm
UPREFIX=/tmp/$(PRJ)/debian/usr
DOCMAIN=./docs/html/main.html

IDIST=../../dists/stable/main/binary-i386


all:
	@echo "Version: ${VERSION}"
	@make -C client
	@make -C client-test        
	@make -C driver   
	@make -C driver-test  
	@make -C monitor

clean:
	@echo "Version: ${VERSION}"
	@make -C client       clean
	@make -C client-test  clean      
	@make -C driver       clean
	@make -C driver-test  clean
	@make -C monitor      clean
	
doc:
	@make -C client       doc


rel: doc deb up


deb:
	@echo "* Building .deb -- Version: ${VERSION}"
	
	@echo "> copying template from 'packages' to '/tmp'"
	@rm -rf /tmp/$(PRJ)
	@mkdir /tmp/$(PRJ)
	@rsync -r --exclude=*.svn* packages/debian /tmp/$(PRJ)
	
	@echo "> cloning required files"
	@cp driver/Debug/erlang-dbus-driver_debug    /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/driver
	@cp driver/Release/erlang-dbus-driver        /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/driver
	
	@cp client/src/erlang-dbus-client.erl        /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/src
	@cp client/src/erlang-dbus-client-server.erl /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/src
	@cp client/src/erlang-dbus-client-tools.erl  /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/src
	
	@cp client/ebin/erlang-dbus-client.beam        /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/ebin
	@cp client/ebin/erlang-dbus-client-server.beam /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/ebin
	@cp client/ebin/erlang-dbus-client-tools.beam  /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/ebin
	
	#@echo "> adusting version info"
	#@./.tools/adjust_version.py $(VERSION) $(UPREFIX)/include/epapi.h $(UPREFIX)/lib/pkgconfig/epapi.pc
	#@./.tools/adjust_version.py $(VERSION) $(CTLFILES)

	#@echo "> adjusting DEBIAN control files permissions"
	#@chmod 0775 $(CTLFILES)

	#@echo "> running dpkg-deb"
	#@dpkg-deb --build /tmp/$(PRJ)/debian /tmp/$(PRJ)/$(PRJ).deb
	
	#@echo "> copying .deb to repository"
	#@cp /tmp/$(PRJ)/$(PRJ).deb $(IDIST)/$(PRJ)_$(VERSION)-$(DEBIAN_PKG_VER)_i386.deb
	
	#@echo "> running dpkg-scanpackages"
	#@dpkg-scanpackages -m $(IDIST) /dev/null | gzip -9c > $(IDIST)/Packages.gz


src:
	@echo "* Making sources archive - version ${VERSION}"
	@echo "> copying tree to /tmp"
	@rm -rf /tmp/$(PRJ)_sources
	@mkdir /tmp/$(PRJ)_sources
	@mkdir /tmp/$(PRJ)_sources/$(PRJ)
	@cp -R ../project/* /tmp/$(PRJ)_sources/$(PRJ)

	@echo "> zipping"
	cd /tmp/$(PRJ)_sources && zip -r $(PRJ).zip $(PRJ) -x *.svn* *.so *.o *.os *html* *.old*
	mv /tmp/$(PRJ)_sources/$(PRJ).zip /tmp/$(PRJ)_sources/$(PRJ)-$(VERSION).zip

up:
	@echo "* Uploading sources archive to GoogleCode"
	@./.tools/upload.py /tmp/$(PRJ)_sources/$(PRJ)-$(VERSION).zip $(PRJ)

	