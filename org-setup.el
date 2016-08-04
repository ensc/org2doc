; note: requires recent org-mode from ELPA (tested with 20150914)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")))

(package-initialize)

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

  (goto-char (point-min))
  (while (re-search-forward "@ORG_GIT_REVISION@" nil t)
    (replace-match
     (replace-regexp-in-string "\n$" ""
			       (shell-command-to-string "${GIT:-git} describe --dirty --always")) t))
  )

(add-hook 'org-export-before-parsing-hook 'ensc/org-fixup-latex-export)

(setq org-latex-listings t
      org-src-fontify-natively t)

;; prevent generation of *~ backup files
(setq backup-inhibited t)
