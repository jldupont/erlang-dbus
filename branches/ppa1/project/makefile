# @author Jean-Lou Dupont
# @title  Erlang-DBus
#
VERSION:=`cat VERSION`
PRJ=erlang-dbus


clean:
	@echo "Version: ${VERSION}"
	@rm -r -f /tmp/$(PRJ)
	
	

deb:
	@echo "* Building .deb -- Version: ${VERSION}"
	@mkdir -p "/tmp/$(PRJ)"
	@mkdir -p "/tmp/$(PRJ)/$(PRJ)-$(VERSION)"	
	@echo "Copying package files"
	@rsync -r --exclude=*.svn* package/* "/tmp/$(PRJ)/$(PRJ)-$(VERSION)"
	@echo "Copying debian folder"
	@rsync -r --exclude=*.svn* packages/debian "/tmp/$(PRJ)/$(PRJ)-$(VERSION)"
	@echo "Updating Version"
	

.PHONY: deb clean
	