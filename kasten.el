;;; kasten.el --- Zettelkasten note browser  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiyang Sun

;; Author: Qiyang Sun <iamsqy@outlook.com>
;; Maintainer: Qiyang Sun <iamsqy@outlook.com>
;; Created: 21 Jun 2025
;; URL: https://github.com/iamsqy/kasten
;; Package-Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "0.33"))
;; Keywords: notes, Zettelkasten


;; This file is not part of GNU Emacs.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:

(require 'consult)
(require 'filenotify)

(defgroup kasten nil
  "Tools for Zettelkasten note taking."
  :group 'tools)

(defcustom kasten-directory (expand-file-name "~/jrn/")
  "Note directory."
  :type 'directory
  :group 'kasten)

(defcustom kasten-file-extensions '("org" "txt" "tex")
  "List of searchable file extensions for Kasten."
  :type '(repeat string)
  :group 'kasten)

(defcustom kasten-search-function #'consult-ripgrep
  "Function used for searching within the Kasten directory."
  :type 'function
  :group 'kasten)

(defcustom kasten-title-regexp "^#\\+[tT][iI][tT][lL][eE]: *\\(.*\\)$"
  "Regexp to match the title of a note."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-title-max-pos 4096
  "Max position of the title of a note, i.e. depth for Kasten to match `kasten-title-regexp'."
  :type 'integer
  :group 'kasten)

(defcustom kasten-tag-regexp "#\\([[:alnum:]_-]+\\)"
  "Regexp to match a tag of a note."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-tag-max-pos 1048576
  "Max position of the title of a note, i.e. depth for Kasten to match `kasten-tag-regexp'."
  :type 'integer
  :group 'kasten)

(defcustom kasten-tag-first-char-regexp "\\x23"
  "Regexp for the tag char to make `kasten-search-function' understand, e.g. `\\x23' for `\#'."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-id-symbol "§"
  "Leading symbol for ID, e.g. `\§' in ID `\§20250229-2333'.")

(defcustom kasten-id-regexp "§\\([0-9]\\{8\\}-[0-9]\\{4\\}\\)"
  "Regexp for an ID in a note."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-buffer-title "Kasten\n"
  "Kasten mode buffer title."
  :type 'string
  :group 'kasten)

(defface kasten-buffer-title-face
  '((t :inherit variable-pitch :weight bold :height 2.33
       :foreground "cyan" :background "navy" :slant italic
       :underline (:color "blue" :style line) :extend t))
  "Face for the Kasten buffer title."
  :group 'kasten)

(defface kasten-file-title-face
  '((t :foreground "gold" :weight bold))
  "Face for file titles in Kasten."
  :group 'kasten)

(defface kasten-file-name-face
  '((t :foreground "green"))
  "Face for filenames in Kasten."
  :group 'kasten)

(defvar kasten-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'kasten-open-file)
    (define-key map (kbd "/") #'kasten-search)
    (define-key map (kbd "g") (lambda ()
				(interactive)
				(kasten-refresh nil nil)))
    map)
  "Keymap for Kasten mode.")

(define-derived-mode kasten-mode special-mode "Kasten"
  "Major mode for browsing notes."
  (setq buffer-read-only t)
  (hl-line-mode 1)
  (when kasten-auto-refresh
    (kasten--enable-auto-refresh))
  (kasten-refresh t t))

(defun kasten-refresh (&optional is-init is-auto)
  "Refresh note list.
Reset point if IS-INIT is non-nil; display message with time lapsed and
according to IS-AUTO."
  (interactive)
  (let ((buffer (get-buffer-create "*Kasten*"))
	(start-time (float-time)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
	    (saved-point (point)))
	(erase-buffer)
	(insert (propertize kasten-buffer-title 'face 'kasten-buffer-title-face))
	(insert "\n")
	(dolist (file (kasten--get-note-files))
	  (let* ((title (kasten-parse-org-title file))
		 (filename (file-name-base file)))
	    (insert (propertize title 'face 'kasten-file-title-face))
	    (insert (make-string
		     (max 2 (- (window-width) (length title) (length filename)))
		     ?\s))
	    (insert (propertize filename 'face 'kasten-file-name-face))
	    (insert "\n")))
	(if (eq is-init t)
	    (progn
	      (goto-char (point-min))
	      (forward-line 2))
	  (goto-char saved-point))
	(if (eq is-auto t)
	    (message "Kasten: automatically updated index in %.6f seconds"
		     (- (float-time) start-time))
	  (message "Kasten: index updated in %.6f seconds"
		   (- (float-time) start-time)))))))

(defvar kasten--watch-handle nil
  "Handle returned by `file-notify-add-watch`.")

(defun kasten--set-auto-refresh (symbol value)
  "Handle change to `kasten-auto-refresh`, set SYMBOL to VALUE."
  (set-default symbol value)
  (if (eq value t)
      (kasten--enable-auto-refresh)
    (kasten--disable-auto-refresh)))

(defun kasten--maybe-auto-refresh (_event)
  "Triggers `kasten-refresh` if `kasten-auto-refresh` is non-nil."
  (when kasten-auto-refresh
    (kasten-refresh nil t)))

(defun kasten--enable-auto-refresh ()
  "Enable auto refresh when files change in `kasten-directory`."
  (when kasten--watch-handle
    (file-notify-rm-watch kasten--watch-handle))
  (setq kasten--watch-handle
        (file-notify-add-watch
         kasten-directory
         '(change attribute-change)
         #'kasten--maybe-auto-refresh)))

(defun kasten--disable-auto-refresh ()
  "Disable automatic Kasten refresh on files change."
  (when kasten--watch-handle
    (file-notify-rm-watch kasten--watch-handle)
    (setq kasten--watch-handle nil)))

(defcustom kasten-auto-refresh t
  "If non-nil, automatically refresh Kasten buffer when files change."
  :type 'boolean
  :group 'kasten
  :set #'kasten--set-auto-refresh)

(defun kasten-parse-org-title (file)
  "Return the Org title from FILE or fallback to base filename."
  (with-temp-buffer
    (insert-file-contents file nil 0 kasten-title-max-pos)
    (let ((contents (buffer-string)))
      (if (string-match kasten-title-regexp contents)
          (string-trim (match-string 1 contents))
        (file-name-base file)))))

(defun kasten--extension-regexp ()
  "Return a regexp matching extensions in `kasten-file-extensions`."
  (concat "\\." (regexp-opt kasten-file-extensions t) "$"))

(defun kasten--get-note-files ()
  "Return a list of note file paths in `kasten-directory`."
  (directory-files-recursively kasten-directory (kasten--extension-regexp)))

(defun kasten-open-file ()
  "Open the note file on the current line by matching filename."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (id (and line (string-trim (car (last (split-string line))))))
         (file (kasten--id-to-file id)))
    (if file
	(progn
          (find-file file)
	  (message "Kasten: found file `%s'" file))
      (message "Kasten: could not open file with ID `%s': not found" filename))))

(defun kasten--id-to-file (id)
  "Find full path of note file matching ID."
  (seq-find (lambda (f)
              (string= (file-name-base f) id))
            (kasten--get-note-files)))

(defun kasten--follow-id-link (id)
  "Open the file corresponding to ID."
  (let ((file (kasten--id-to-file id)))
    (if file
        (find-file file)
      (message "Kasten: could not follow ID `%s': file not found" id))))

(defun kasten--fontify-clickable-id ()
  "Make Kasten ID clickable in buffers."
  (font-lock-add-keywords
   nil
   `((,kasten-id-regexp
      (0 (progn
           (let* ((id (match-string-no-properties 1))
                  (map (make-sparse-keymap)))
             (define-key map [mouse-1]
			 `(lambda () (interactive) (kasten--follow-id-link ,id)))
             (define-key map (kbd "RET")
			 `(lambda () (interactive) (kasten--follow-id-link ,id)))
             (add-text-properties
              (match-beginning 0) (match-end 0)
              `(mouse-face highlight
                           help-echo "Mouse-1 or RET: Follow Kasten ID"
                           face org-link
                           keymap ,map))))
         nil))))
  (font-lock-flush))

(add-hook 'org-mode-hook #'kasten--fontify-clickable-id)

(defun kasten-insert-id ()
  "Prompt to insert an ID referencing a note."
  (interactive)
  (let* ((files (kasten--get-note-files))
         (ids (mapcar #'file-name-base files))
         (id (completing-read "Insert Kasten ID: " ids nil nil)))
    (when (and id (not (string-empty-p id)))
      (insert (concat kasten-id-symbol id)))))

(defun kasten-search ()
  "Search using `kasten-search-function`."
  (interactive)
  (funcall kasten-search-function kasten-directory))

(defun kasten-search-tag ()
  "Prompt and search for a tag."
  (interactive)
  (let* ((tags (kasten--collect-tags))
         (tag (completing-read "Search tag: " tags nil nil)))
    (when tag
      (funcall kasten-search-function
	       kasten-directory
	       (concat kasten-tag-first-char-regexp
		       (regexp-quote (substring tag 1))
		       "\\b")))))

(defun kasten--collect-tags ()
  "Return a list of unique tags found in all notes."
  (let ((files (kasten--get-note-files))
        (tags '()))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file nil 0 kasten-tag-max-pos)
        (goto-char (point-min))
        (while (re-search-forward kasten-tag-regexp nil t)
          (push (match-string-no-properties 0) tags))))
    (delete-dups tags)))

(defun kasten-insert-tag ()
  "Prompt to insert an existing or new tag at point."
  (interactive)
  (let* ((tags (kasten--collect-tags))
         (tag (completing-read "Insert tag: " tags nil nil)))
    (insert tag)))

(defun kasten-show-backlinks-current-note ()
  "Show all notes that link to the current note by ID."
  (interactive)
  (let* ((file (buffer-file-name))
         (id (and file (file-name-base file))))
    (if (not id)
        (message "Kasten: not visiting a note file.")
      (funcall kasten-search-function
	       kasten-directory (concat kasten-id-symbol (regexp-quote id))))))

(defun kasten-show-backlinks (id)
  "Prompt for an ID and show backlinks to it across the notes."
  (interactive
   (let* ((ids (mapcar #'file-name-base (kasten--get-note-files)))
          (choice (completing-read "Backlinks to ID: " ids nil nil)))
     (list choice)))
  (funcall kasten-search-function
	   kasten-directory (concat kasten-id-symbol (regexp-quote id))))

;;;###autoload
(defun kasten ()
  "Launch Kasten major mode."
  (interactive)
  (switch-to-buffer "*Kasten*")
  (kasten-mode))

(provide 'kasten)

;;; kasten.el ends here
