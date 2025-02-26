#
# make release
#
APP = candy
APPL = Candy
VSN = $(shell git describe --abbrev=0)
MACHINE = $(shell uname -m)
CONFIG = -config $(APP).config

all:
	@if [ -d "src" -a -f "src/Makefile" ]; then (cd src && $(MAKE) all); fi
	@if [ -d "c_src" -a -f "c_src/Makefile" ]; then (cd c_src && $(MAKE) all); fi
	@if [ -d "test" -a -f "test/Makefile" ]; then (cd test && $(MAKE) all); fi

clean:
	@if [ -d "src" -a -f "src/Makefile" ]; then (cd src && $(MAKE) clean); fi
	@if [ -d "c_src" -a -f "c_src/Makefile" ]; then (cd c_src && $(MAKE) clean); fi
	@if [ -d "test" -a -f "test/Makefile" ]; then (cd test && $(MAKE) clean); fi

update_vsn:
	rm ebin/$(APP).app
	(cd src; make ../ebin/$(APP).app)

appimage:	update_vsn
	erl -epx -noshell -s $(APP) start0 $(CONFIG) -s servator make_appimage $(APP) -s erlang halt
	(cd $(APP).AppDir; ../../servator/priv/make_desktop_icons $(APP).png)
	strip $(APP).AppDir/bin/beam.smp
	strip $(APP).AppDir/bin/epmd
	strip $(APP).AppDir/bin/erlc
	strip $(APP).AppDir/bin/erl_child_setup
	strip $(APP).AppDir/bin/erlexec
	strip $(APP).AppDir/bin/escript
	strip $(APP).AppDir/bin/heart
	strip $(APP).AppDir/bin/inet_gethost
	appimagetool -n $(APP).AppDir
	mv $(APPL)-$(MACHINE).AppImage $(APPL)-$(VSN)-$(MACHINE).AppImage

osxapp:	update_vsn
	erl -sname candy -epx -noshell -s $(APP) $(CONFIG) -s servator make_osxapp $(APP) -s erlang halt
	mkdir -p tmpdist
	mv $(APPL).app tmpdist/
	cd tmpdist/
	../../servator/priv/make_icns ../priv/$(APP).png
	rm -rf AppIcon.iconset
	mv AppIcon.icns Varp.app/Contents/Resources/
	cd ..
	hdiutil create tmp.dmg -ov -volname "$(APPL)" -fs HFS+ -srcfolder "./tmpdist/"
	hdiutil convert -format UDZO -o $(APPL).dmg tmp.dmg
