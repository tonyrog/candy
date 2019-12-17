#
# make release
#
APP = candy
APPL = Candy
VSN = $(shell git describe)
MACHINE = $(shell uname -m)

appimage:
	erl -epx -noshell -s candy -config candy.config -s servator make_appimage $(APP) -s erlang halt
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

osxapp:
	erl -epx -noshell -s $(APP) -s servator make_osxapp $(APP) -s erlang halt
	mkdir -p tmpdist
	mv $(APPL).app tmpdist/
	cd tmpdist/
	../../servator/priv/make_icns ../priv/$(APP).png
	rm -rf AppIcon.iconset
	mv AppIcon.icns Varp.app/Contents/Resources/
	cd ..
	hdiutil create tmp.dmg -ov -volname "$(APPL)" -fs HFS+ -srcfolder "./tmpdist/"
	hdiutil convert -format UDZO -o $(APPL).dmg tmp.dmg
