EMACS=emacs

EL=tnt.el toc.el tocstr.el
ELC = $(EL:.el=.elc)

RELEASE=$(EL) $(ELC) INSTALL README PROTOCOL ChangeLog TODO Makefile


.SUFFIXES: .el .elc


LOADPATH =-eval '(setq load-path (cons "." load-path))'

.el.elc:
	$(EMACS) -batch $(LOADPATH) -q -f batch-byte-compile $<

# Default rule is simply to compile all .el's
elc: $(ELC)


VERSION_PATTERN=^(defconst tnt-version "TNT \([^\"]*\)")
release: $(RELEASE)
	@chmod -x PROTOCOL
	@chmod -x README
	@NAME=`sed -n 's/$(VERSION_PATTERN)/tnt-\1/p' tnt.el` ;    \
	echo Creating $${NAME}.tar.gz ;                            \
	rm -f $$NAME; ln -s . $$NAME; tar vzchf $${NAME}.tar.gz    \
        `echo $(RELEASE) | sed "s|^|$$NAME/|g; s| | $$NAME/|g" ` ; \
	rm $$NAME

clean:
	rm -rf $(ELC) tnt-*.tar.gz
