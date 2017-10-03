; note: requires recent org-mode from ELPA (tested with 20150914)

(require 'cl)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(defmacro ensc/babel-by-backend (&rest body)
  `(case (when (boundp 'org-export-current-backend)
	     org-export-current-backend) ,@body))

(defun ensc/package-install (pkg version archive kind)
  (let ((p (package-desc-create :name pkg
				:version version
				:archive archive
				:kind kind)))
    (package-install p)))

(defun ensc/shell-first-line (cmd)
  ;; TODO: semantic is wrong; we should return the first line only but
  ;; strip trailing \n instead of.
  (replace-regexp-in-string "\n+$" "" (shell-command-to-string cmd) t))

(defun ensc/org-fixup-latex-export (backend)
  (require 'cl)
  (goto-char (point-min))
  (while (re-search-forward "^\\([[:space:]]*|\\([[:space:]]*|\\)?[[:space:]]*\\)\\(\\*+\\)\\([[:space:]]+\\)\\(<<[-_+[:alnum:]]*>>\\)?" nil t)
    (case backend
      (latex (replace-match (format "%s\\\\latextblheading{%s}%s"
				    (match-string 1)
				    (length (match-string 3))
				    (match-string 4))))
      (html (replace-match (format "%s%s" ; "%s<span class='tblspace%s'></span>%s"
				   (match-string 1)
				   (let ((tmp ""))
				     (dotimes (i (length (match-string 3)) tmp)
				       (setq tmp (concat tmp "\\\\nbsp{}")))))))
      ))

  (let* ((rev (ensc/shell-first-line "${GIT:-git} describe --dirty --always"))
	 (cnt (ensc/shell-first-line "${GIT:-git} rev-list HEAD | wc -l"))
	 (longrev (ensc/shell-first-line "${GIT:-git} rev-parse HEAD"))
	 (revstr (concat cnt "\\small{+}g" rev))
	 (subst `(("@ORG_GIT_REVISION@"     . ,revstr)
		  ("@ORG_GIT_REVISION_CNT@" . ,cnt)
		  ("@ORG_GIT_REVISION_LONG@" . ,longrev)
		  ("@ORG_GIT_REVISION_REV@" . ,rev)))
	 (regexp (regexp-opt (cl-mapcar 'car subst))))

    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match (cdr (assoc (match-string 0) subst)) t t))))

(add-hook 'org-export-before-parsing-hook 'ensc/org-fixup-latex-export)

(setq org-latex-listings t
      org-src-fontify-natively t
      org-latex-caption-above nil
      org-html-table-caption-above nil
      org-ascii-caption-above nil)

;; prevent generation of *~ backup files
(setq backup-inhibited t)

(setq org-babel-load-languages '((emacs-lisp . t)
				 (mscgen . t)
				 (latex . t))
      org-babel-latex-htlatex "htlatex"
      org-confirm-babel-evaluate nil)

(autoload 'org-html-export-to-html "ox-html")
(autoload 'org-latex-export-to-latex "ox-latex")
