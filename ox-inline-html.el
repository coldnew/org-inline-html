;;; ox-inline-html.el --- Export org-mode to HTML with inline image and css

;; Copyright (c) 2018 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/org-inline-html
;; Version: 0.1
;; Package-Requires: ((org "9.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ox-html)


;;;; Group and Customize options

(defgroup org-export-inline-html nil
  "Options for exporting Org mode files to MHTML format."
  :tag "Org Export to HTML with inline image and css."
  :group 'org-export-inline-html
  :link '(url-link :tag "Github" "https://github.com/coldnew/org-inline-html"))

(defcustom org-inline-html-extension "html"
  "The extension for exported HTML files with inline images and css."
  :group 'org-export-inline-html
  :type 'string)


;;;; Backend

(org-export-define-derived-backend 'inline-html 'html
  :translate-alist
  '(;; drop most of nouse html header
    ;; (template . org-hexo-html-template)
    ;; Fix for multibyte language
    (paragraph . org-inline-html-paragraph)
    ;; Fix toc for org-hexo theme
    ;; (inner-template . org-hexo-html-inner-template)
    ;; convert relative link to let pelican can recognize
    (link . org-inline-html-link)
    )
  ;; :options-alist org-mhtml-options-alist
  )

;;;; Links

(defun org-inline-html-link (link desc info)
  "Transcode a LINK object from Org to MHTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'.
In this function, we also add link file"
  (let ((org-html-link-org-files-as-html nil)
        (html-link (org-html-link link desc info)))
    ;; replace images in html and encoding to base64
    (replace-regexp-in-string
     "src=\"\\([^\"]+\\)\""
     (lambda (text)
       (let* ((url (and (string-match "src=\"\\([^\"]+\\)\"" text)
                        (match-string 1 text)))
              (path (replace-regexp-in-string "^file://" "" url))
              (ext (file-name-extension path)))
         (format "src=\"data:image/%s;base64,%s\" %s"
                 ext
                 (base64-encode-string
                  (with-temp-buffer
                    (insert-file-contents-literally path)
                    (buffer-string)))
                 (file-name-nondirectory path))))
     html-link)))

;;;; Paragraph

(defun org-inline-html-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into MHTML format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let* (;; Fix multibyte language like chinese will be automatically add
         ;; some space since org-mode will transpose auto-fill-mode's space
         ;; to newline char.
         (fixed-regexp "[[:multibyte:]]")
         (fixed-contents
          (replace-regexp-in-string
           (concat "\\(" fixed-regexp "\\) *\n *\\(" fixed-regexp "\\)") "\\1\\2" contents)))
    ;; Add our changes to org-html-paragraph
    (org-html-paragraph paragraph fixed-contents info)))


;;;; End-user functions


;;;###autoload
(defun org-inline-html-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an inline HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Inline HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'inline-html "*Org Inline HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (html-mode))))

;;;###autoload
(defun org-inline-html-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
                                    org-inline-html-extension
                                    "html")))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'dhtml file
      async subtreep visible-only body-only ext-plist)))


(provide 'ox-inline-html)
;;; ox-inline-html.el ends here
