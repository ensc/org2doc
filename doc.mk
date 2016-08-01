LATEXMK =	latexmk
PDFLATEX =	env TEXINPUTS=${srcdir}: pdflatex
PDFLATEX_FLAGS = \
	-file-line-error -halt-on-error \
	-interaction nonstopmode -shell-escape \

EMACS_ENV = \
	HOME=${abs_builddir} \
	GNUPGHOME=${abs_builddir}/.emacs.d/.gnupg

EMACS = \
	env ${EMACS_ENV} \
	emacs --batch --kill -Q -nw -l ${srcdir}/org-setup.el

RUN_LATEX = \
	${LATEXMK} -pdflatex='${PDFLATEX} ${PDFLATEX_FLAGS} %O %S'

srcdir ?= 	$(dir $(firstword ${MAKEFILE_LIST}))
builddir =	.
abs_srcdir =	$(abspath ${srcdir})
abs_builddir =	$(abspath ${builddir})

ORG_SOURCES ?=	README

all:

clean:		clean-doc

clean-doc:
	rm -f *.aux *.fdb_latexmk *.fls *.toc *.*~ texput.log .*.deps
	rm -f $(foreach o,${ORG_SOURCES},$(prefix $o,.log .out .tex* .pdf .html))

mrproper:	mrproper-doc

mrproper-doc:	clean-doc
	rm -rf .emacs.d auto

.emacs.d/.stamp-downloaded:	 ${srcdir}/org-setup.el
	@mkdir -p ${@D}
	${EMACS} -f package-refresh-contents
	@touch $@

.emacs.d/.stamp-pkg-%:		.emacs.d/.stamp-downloaded
	${EMACS} --eval "(package-install '$*)"
	@touch $@

$(addsuffix .html,${ORG_SOURCES}):%.html:	%.org .emacs.d/.stamp-pkg-htmlize .emacs.d/.stamp-pkg-org
	${EMACS} $< -f org-html-export-to-html

$(addsuffix .tex,${ORG_SOURCES}):%.tex:		%.org .emacs.d/.stamp-pkg-org
	${EMACS} $< -f org-latex-export-to-latex

$(addsuffix .pdf,${ORG_SOURCES}):%.pdf:		%.tex
	${RUN_LATEX} -pdf $< -deps-out=${@D}/.${@F}.deps.tmp
	${RUN_LATEX} -c -pdf $<
	# Standard dep generator lists the .deps file too which makes
	# e.g. 'clean' to generate everytime the .tex file.  Filter it
	# out.
	@sed -e 's!^$@.*!$@:\\!' ${@D}/.${@F}.deps.tmp > ${@D}/.${@F}.deps
	@rm -f -- ${@D}/.${@F}.deps.tmp

-include $(addsuffix .pdf.defs,$(addprefix .,${ORG_SOURCES}))
