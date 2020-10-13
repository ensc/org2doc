WGET =		wget
UNZIP =		unzip
LATEXMK =	latexmk
PDFLATEX =	env TEXINPUTS=${srcdir}: pdflatex
PDFLATEX_FLAGS = \
	-file-line-error -halt-on-error \
	-interaction nonstopmode -shell-escape \

EMACS_ENV = \
	HOME=${abs_builddir} \
	GNUPGHOME=${abs_builddir}/.emacs.d/.gnupg \
	TEXINPUTS=${_texinputs}

EMACS = \
	env ${EMACS_ENV} \
	emacs --batch --kill -Q -nw \
	$(addprefix -l ,${_setup_el})

RUN_LATEX = \
	${LATEXMK} -pdflatex='${PDFLATEX} ${PDFLATEX_FLAGS} %O %S'

GPG =		$(shell type -p gpg2 || echo gpg)
ELPA_KEY =	066DAFCB81E42C40.asc

srcdir ?= 	$(dir $(firstword ${MAKEFILE_LIST}))
builddir =	.
abs_srcdir =	$(abspath ${srcdir})
abs_builddir =	$(abspath ${builddir})

ORG_SOURCES ?=	README

ORG_VERSION	?= 9 4
HTMLIZE_VERSION ?= 20200816 746

LOCALSETUP_EL   ?= $(wildcard ${builddir}/local-setup.el)
PKGSPEC_org      = $(call pkgspec, org,     ${ORG_VERSION},     gnu,   tar)
PKGSPEC_htmlize  = $(call pkgspec, htmlize, ${HTMLIZE_VERSION}, melpa, single)

DITAA_VERSION	?= 0.9
DITAA_ZIP	?= https://sourceforge.net/projects/ditaa/files/ditaa/${DITAA_VERSION}/ditaa0_9.zip

_setup_el	 = ${srcdir}/org-setup.el ${LOCALSETUP_EL}

_texinputs = $(abspath ${*D}):${abs_srcdir}:
pkgspec = \'$(strip $1)\ \'\('$(strip $2)'\)\ \"$(strip $3)\"\ \'$(strip $4)

all:

clean:		clean-doc

clean-doc:
	rm -f *.aux *.fdb_latexmk *.fls *.toc *.*~ texput.log .*.deps
	rm -f $(foreach o,${ORG_SOURCES},$(addprefix $o,.org-recalc .log .out .tex* .pdf .html))
	rm -f $(patsubst %,auto/%.el,${ORG_SOURCES})
	-rmdir auto

mrproper:	mrproper-doc

mrproper-doc:	clean-doc
	rm -rf .emacs.d auto

.emacs.d/.stamp-downloaded:	 ${srcdir}/org-setup.el
	@mkdir -p ${@D}
	env ${EMACS_ENV} ${GPG} --import "${srcdir}/${ELPA_KEY}"
	${EMACS} -f package-refresh-contents
	@touch $@

##### {{{ ditaa support
ifneq (${ENABLE_DITAA},)
.emacs.d/.stamp-pkg-org:	| .emacs.d/.stamp-extra-ditaa
mrproper:	mrproper-ditaa
endif

.emacs.d/.stamp-extra-ditaa:	.emacs.d/elpa/contrib/scripts/ditaa.jar
.emacs.d/elpa/contrib/scripts/ditaa.jar:%/ditaa.jar:	%/ditaa.zip
	@rm -f '${@D}'/ditaa*.jar '$@'
	${UNZIP} -p '$(abspath $<)' 'ditaa*.jar' > '$@'.tmp
	mv '$@'.tmp '$@'

%/ditaa.zip:
	@mkdir -p ${@D}
	@rm -f $@ $@.tmp
	${WGET} '${DITAA_ZIP}' -O $@.tmp
	mv $@.tmp $@

mrproper-ditaa:
	-rm -f .emacs.d/elpa/contrib/scripts/ditaa*
	-rmdir .emacs.d/elpa/contrib/scripts .emacs.d/elpa/contrib
##### }}} ditaa support

.emacs.d/.stamp-pkg-%:		| .emacs.d/.stamp-downloaded
	${EMACS} --eval '(ensc/package-install '${PKGSPEC_$*}')'
	@touch $@

# .SECONDARY would be better but is not supported
.PRECIOUS:	.emacs.d/.stamp-pkg-%
.SECONDARY:	.emacs.d/.stamp-downloaded

%.org-recalc:	%.org .emacs.d/.stamp-pkg-org
	rm -f $@ $@.tmp
	${EMACS} $< --eval '(progn (org-dblock-update t)(org-table-iterate-buffer-tables)(write-file "$@.tmp"))'
	mv $@.tmp $@

$(addsuffix .html,${ORG_SOURCES}):%.html:	%.org-recalc .emacs.d/.stamp-pkg-htmlize
	${EMACS} $< -f org-html-export-to-html

$(addsuffix .tex,${ORG_SOURCES}):%.tex:		%.org-recalc
	${EMACS} $< -f org-latex-export-to-latex

$(addsuffix .txt,${ORG_SOURCES}):%.txt:		%.org-recalc
	${EMACS} $< -f org-ascii-export-to-ascii

$(addsuffix .odt,${ORG_SOURCES}):%.odt:		%.org-recalc
	${EMACS} $< -f org-odt-export-to-odt

$(addsuffix .pdf,${ORG_SOURCES}):%.pdf:		%.tex
	${RUN_LATEX} -pdf $< -deps-out=${@D}/.${@F}.deps.tmp
	${RUN_LATEX} -c -pdf $<
	# Standard dep generator lists the .deps file too which makes
	# e.g. 'clean' to generate everytime the .tex file.  Filter it
	# out.
	@sed -e 's!^$@.*!$@:\\!' ${@D}/.${@F}.deps.tmp > ${@D}/.${@F}.deps
	@rm -f -- ${@D}/.${@F}.deps.tmp

-include $(addsuffix .pdf.defs,$(addprefix .,${ORG_SOURCES}))
