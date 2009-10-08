#
# @author Jean-Lou Dupont
#
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
	@cp -r packages/debian /tmp/$(PRJ)
	
	@echo "> cloning required files"
	@cp lib/include/epapi.h                 /tmp/$(PRJ)/debian/usr/include/epapi.h
	@cp lib/include/epapi_msg.h             /tmp/$(PRJ)/debian/usr/include/epapi_msg.h
	@cp lib/include/epapi_pkt.h             /tmp/$(PRJ)/debian/usr/include/epapi_pkt.h
	@cp lib/include/epapi_term.h            /tmp/$(PRJ)/debian/usr/include/epapi_term.h
	@cp lib/Debug/libepapi_debug.so         /tmp/$(PRJ)/debian/usr/lib/libepapi_debug.so-$(VERSION)
	@cp lib/Release/libepapi.so             /tmp/$(PRJ)/debian/usr/lib/libepapi.so-$(VERSION)
	@cp loop-drv/Debug/epapi_loop_drv_debug /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/ebin/epapi_loop_drv_debug
	@cp loop-drv/Release/epapi_loop_drv     /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/ebin/epapi_loop_drv
	@cp loop-test/ebin/test.beam            /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/ebin/test.beam
	@cp loop-test/src/test.erl              /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/src/test.erl

	@echo "> adusting version info"
	@./.tools/adjust_version.py $(VERSION) $(UPREFIX)/include/epapi.h $(UPREFIX)/lib/pkgconfig/epapi.pc
	@./.tools/adjust_version.py $(VERSION) $(CTLFILES)

	@echo "> adjusting DEBIAN control files permissions"
	@chmod 0775 $(CTLFILES)

	@echo "> running dpkg-deb"
	@dpkg-deb --build /tmp/$(PRJ)/debian /tmp/$(PRJ)/$(PRJ).deb
	
	@echo "> copying .deb to repository"
	@cp /tmp/$(PRJ)/$(PRJ).deb $(IDIST)/$(PRJ)_$(VERSION)-$(DEBIAN_PKG_VER)_i386.deb
	
	@echo "> running dpkg-scanpackages"
	@dpkg-scanpackages -m $(IDIST) /dev/null | gzip -9c > $(IDIST)/Packages.gz


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

	