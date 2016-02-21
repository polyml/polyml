AUTOMAKE_OPTIONS=foreign

all-local: testmod

clean-local:
	-rm -f testmod

install-exec-local:
	$(mkdir_p) $(DESTDIR)$(moduledir)
	$(INSTALL_PROGRAM) testmod $(DESTDIR)$(moduledir)

uninstall-local:
	-rm -f $(DESTDIR)$(moduledir)/testmod

testmod: ../../poly ROOT.sml
	echo "use \"$(srcdir)/ROOT.sml\";" | ../../poly -q -error-exit
