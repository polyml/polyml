AUTOMAKE_OPTIONS=foreign

all-local: intasintinf

clean-local:
	-rm -f intasintinf

install-exec-local:
	$(mkdir_p) $(DESTDIR)$(moduledir)
	$(INSTALL_PROGRAM) intasintinf $(DESTDIR)$(moduledir)

uninstall-local:
	-rm -f $(DESTDIR)$(moduledir)/intasintinf

intasintinf: ../../poly ROOT.sml
	echo "use \"$(srcdir)/ROOT.sml\";" | ../../poly -q -error-exit
