;;; kasten.el --- Zettelkasten note browser  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiyang Sun

;; Author: Qiyang Sun <iamsqy@outlook.com>
;; Maintainer: Qiyang Sun <iamsqy@outlook.com>
;; Created: 21 Jun 2025
;; URL: https://github.com/iamsqy/kasten
;; Package-Version: 0.2
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

(defcustom kasten-default-extension "org"
  "Default extension for new notes."
  :type 'string
  :group 'kasten)

(defcustom kasten-note-template "#+title: "
  "Template for new notes."
  :type 'string
  :group 'kasten)

(defcustom kasten-index-hidden-files nil
  "If non-nil, include dot files when indexing.
May cause problem if backup files present in the directory."
  :type 'boolean
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
  "Max position of the title of a note, i.e. depth for Kasten to match \
`kasten-title-regexp'."
  :type 'integer
  :group 'kasten)

(defcustom kasten-category-regexp
  "^#\\+[cC][aA][tT][eE][gG][oO][rR][yY]: *\\(.*\\)$"
  "Regexp to match the category of a note."
  :type 'regexp
  :group 'kasten)


(defcustom kasten-empty-category-placeholder ""
  "If non-empty, display in category field for uncategorised notes.  Leave \
empty to omit empty category for uncategorised notes."
  :type 'string
  :group 'kasten)


(defcustom kasten-category-max-pos 4096
  "Max position of the category of a note, i.e. depth for Kasten to match \
`kasten-category-regexp'."
  :type 'integer
  :group 'kasten)

(defcustom kasten-title-category-split " --- "
  "Split title and category in Kasten view."
  :type 'string
  :group 'kasten)

(defcustom kasten-tag-regexp "#\\([[:alnum:]_-]+\\)"
  "Regexp to match a tag of a note."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-tag-max-pos 65536
  "Max position of the title of a note, i.e. depth for Kasten to match \
`kasten-tag-regexp'."
  :type 'integer
  :group 'kasten)

(defcustom kasten-tag-symbol "#"
  "Leading symbol for tag."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-tag-first-char-regexp "\\x23"
  "Regexp for the tag char to make `kasten-search-function' understand, e.g. \
`\\x23' for `\#'."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-id-symbol "§"
  "Leading symbol for ID, e.g. `\§' in ID `\§250229-2333'."
  :type 'string
  :group 'kasten)

(defcustom kasten-id-timeformat "%y%m%d-%H%M"
  "Time format for generating ID, e.g. `%y%m%d-%H%M' for `250229-2333'.  \
See strftime."
  :type 'string
  :group 'kasten)

(defcustom kasten-folder-timeformat "%Y%m"
  "Time format for folder, e.g. `%Y%m' for folder `202502/'.  See strftime."
  :type 'string
  :group 'kasten)

(defcustom kasten-id-clash-time-inc 60
  "How much time should be increment if the time-based ID clashes.  \
60 for 1 minute."
  :type 'integer
  :group 'kasten)

(defcustom kasten-id-regexp "§\\([0-9]\\{6\\}-[0-9]\\{4\\}\\)"
  "Regexp for an ID in a note."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-backlink-comment "#+backlink: "
  "How to note backlink ID in new note when creating new note at point."
  :type 'string
  :group 'kasten)

(defcustom kasten-buffer-title "Kasten\n"
  "Kasten mode buffer title."
  :type 'string
  :group 'kasten)

(defcustom kasten-minor-mode-lighter " Kt"
  "Kasten minor mode lighter."
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

(defvar kasten-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "H-k") #'kasten) ;;TODO
    map)
  "Keymap for Kasten minor mode.")

(define-derived-mode kasten-mode special-mode "Kasten"
  "Major mode for browsing notes."
  (setq buffer-read-only t)
  (hl-line-mode 1)
  (when kasten-auto-refresh
    (kasten--enable-auto-refresh))
  (kasten-refresh t t))

(define-minor-mode kasten-minor-mode
  "Minor mode for extra features in Kasten buffers."
  :init-value nil
  :lighter kasten-minor-mode-lighter
  :keymap kasten-minor-mode-map
  :group 'kasten
  
  (if kasten-minor-mode
      (progn
        (unless (derived-mode-p 'kasten-mode 'text-mode)
	  (display-warning
	   'kasten-minor-mode
	   (format "Kasten minor mode is not intended for `%s'. \
It is designed for text-mode buffers. The major mode of the current buffer is \
not derived from text-mode. Answer `y' if you want to treat `%s' as note."
		   major-mode (buffer-name))
	   :warning)
          (unless (y-or-n-p
		   (format "[Kasten] Really enable Kasten minor mode in `%s'? "
			   major-mode))
            (setq kasten-minor-mode nil)
            (message "[Kasten] Kasten minor mode not enabled."))))))

(defvar kasten-filters
  '(:title ()
    :category ()
    :mode and)
  "Filters for kasten-refresh filtering of notes.
:title is a list of substrings to match in the title.
:category is a list of categories for exact matching.
:mode is either 'or or 'and to combine the title and category filters.")

(defun kasten-filters-active-p ()
  "Return non-nil if `kasten-filters` has active filters."
  (let ((titles (plist-get kasten-filters :title))
        (categories (plist-get kasten-filters :category)))
    (or titles categories)))

(defun kasten--matches-filter-p (title category)
  "Return t if TITLE and CATEGORY pass the filters in `kasten-filters`."
  (let* ((title-filters (plist-get kasten-filters :title))
         (category-filters (plist-get kasten-filters :category))
         (mode (plist-get kasten-filters :mode))
         (title-match (or (null title-filters)
                          (seq-some (lambda (substr)
                                      (string-match-p (downcase substr)
						      (downcase title)))
                                    title-filters)))
         (category-match (or (null category-filters)
                             (member category category-filters))))
    (pcase mode
      ('or (or title-match category-match))
      ('and (and title-match category-match))
      (_ (user-error "Kasten: invalid mode (not `and' or `or')")))))

(defvar kasten-filters-buffer-name "*Kasten Filters*")

(defvar kasten-filters-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'kasten-filters-save-and-kill)
    (define-key map (kbd "C-c C-k") #'kill-buffer-and-window)
    (define-key map (kbd "C-c C-t") #'kasten-filters--toggle-the-mode)
    map)
  "Keymap for `kasten-filters-mode'.")

(defvar kasten-filters-font-lock-keywords
  '(("^%.*$" . font-lock-comment-face)
    ("^#.*$" . font-lock-keyword-face))
  "Font lock keywords for `kasten-filters-mode'.")

(define-derived-mode kasten-filters-mode text-mode "Kasten-Filters"
  "Major mode for editing `kasten-filters`."
  (setq buffer-read-only nil)
  (setq font-lock-defaults '(kasten-filters-font-lock-keywords))
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[%#].*$" nil t)
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(read-only t front-sticky t rear-nonsticky t)))))
  (setq-local header-line-format (substitute-command-keys "[Kasten] edit \
filters.  \\[kasten-filters-save-and-kill] to save.  \\[kill-buffer-and-window]\
 to discard.")))

(add-hook 'kasten-filters-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq truncate-lines t)))

(defun kasten-filters--insert-current ()
  "Insert the current `kasten-filters` contents in the editing buffer."
  (let ((title-list (plist-get kasten-filters :title))
        (category-list (plist-get kasten-filters :category))
        (mode (plist-get kasten-filters :mode)))
    (insert "\
% To customise Kasten filters, edit this buffer.  When done, `C-c C-c' to save\n")
    (insert "\
% or `C-c C-k' to discard.  Lines starting with `\%' are comments.\n")
    (insert "\
% Title filters: one per line, case insensitive, Emacs regexp supported.\n")
    (insert "# title\n")
    (let ((tail title-list))
      (while tail
        (let ((elem (car tail)))
          (insert elem "\n")
          (setq tail (cdr tail)))))
    (when (null title-list) (insert "\n"))
    (insert "\
% Category filters: one per line, case sensitive.\n")
    (insert "# category\n")
    (let ((tail category-list))
      (while tail
        (let ((elem (car tail)))
          (insert elem "\n")
          (setq tail (cdr tail)))))
    (when (null category-list) (insert "\n"))
    (insert "\
% And/or: `and' for single filter; set point and `C-c C-t' to toggle.\n")
    (insert "# mode\n")
    (insert (symbol-name mode) "\n")
    (insert "% Kasten filter customisation ends here.\n")))

(defun kasten-filters-edit ()
  "Open a buffer for editing the `kasten-filters` variable."
  (interactive)
  (let ((buf (get-buffer-create kasten-filters-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (kasten-filters--insert-current)
      (kasten-filters-mode))
    (pop-to-buffer buf)))

(defun kasten-filters--toggle-the-mode ()
  "Toggle the mode on the current line between `or' and `and' if applicable."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (trimmed (and line (string-trim line))))
    (when (and trimmed (member trimmed '("or" "and")))
      (let ((new-mode (if (string= trimmed "or") "and" "or")))
        (delete-region (line-beginning-position) (line-end-position))
        (insert new-mode)))))

(defun kasten-filters-save-and-kill ()
  "Parse buffer contents and save back to `kasten-filters`, then kill buffer."
  (interactive)
  (let ((title-list '())
        (category-list '())
        mode
        (state :title)
	(mode-set nil)) ; :title, :category, :mode
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim (thing-at-point 'line t))))
          (cond ((string-prefix-p "#" line)
            (cond
             ((string-match-p "# title" line) (setq state :title))
             ((string-match-p "# category" line) (setq state :category))
             ((string-match-p "# mode" line) (setq state :mode))))
           ((= (length line) 0)) ; skip empty line
	   ((string-prefix-p "%" line)) ; skip comments
           (t
            (pcase state
              (:title (push line title-list))
              (:category (push line category-list))
              (:mode (unless mode-set
                       (setq mode (intern line))
                       (setq mode-set t))))))
          (forward-line 1))))
    (setq title-list (nreverse title-list))
    (setq category-list (nreverse category-list))
    (unless (member mode '(or and))
      (user-error "Kasten: Mode must be 'or or 'and, got %s" mode))
    (setq kasten-filters
	  `(:title ,title-list :category ,category-list :mode ,mode))
    (kasten-refresh nil nil)
    (kill-buffer-and-window)))

(defun kasten-refresh (&optional is-init is-auto)
  "Refresh note list.
Reset point if IS-INIT is non-nil; display message with time lapsed and \
according to IS-AUTO."
  (interactive)
  (let ((buffer (get-buffer-create "*Kasten*"))
	(start-time (float-time)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
	    (saved-point (point))
	    (files (kasten--get-note-files)))
	(erase-buffer)
	(insert (propertize kasten-buffer-title 'face 'kasten-buffer-title-face))
	(insert "\n")
	(dolist (file files)
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
	(let ((elapsed (- (float-time) start-time))
	      (file-count (length files)))
          (if (eq is-auto t)
	      (message "Kasten: automatically updated index of %d files in \
%.6f seconds"
		       file-count elapsed)
	    (message "Kasten: index of %d files updated in %.6f seconds"
		     file-count elapsed)))))))

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

(defun kasten--generate-id-and-path ()
  "Generate ID and path, increment time if clash."
  (let ((n 0)
        id path)
    (cl-loop
     with now = (current-time)
     for offset-time = (time-add
			now (seconds-to-time (* kasten-id-clash-time-inc n)))
     for base = (format-time-string kasten-id-timeformat offset-time)
     for dir = (format-time-string kasten-folder-timeformat offset-time)
     for rel-path = (format "%s/%s" dir base)
     do (setq
         id base
         path (expand-file-name rel-path kasten-directory))
     until (not (cl-some
                 (lambda (ext)
                   (file-exists-p (format "%s.%s" path ext)))
                 kasten-file-extensions))
     do (setq n (1+ n)))
    (list :id id :path path)))

(defun kasten-create-new-note ()
  "Create and open a new Kasten note."
  (interactive)
  (pcase-let* ((`(:id ,id :path ,path) (kasten--generate-id-and-path))
               (full-path (concat path "." kasten-default-extension))
               (dir (file-name-directory full-path)))
    (unless (file-directory-p dir)
      (progn
	(make-directory dir t)
	(message "Kasten: created directory `%s'" dir)))
    (find-file full-path)
    (insert kasten-note-template)
    (save-buffer)
    (message "Kasten: created new note `%s' at `%s'" id full-path)))

(defun kasten-create-new-note-at-point ()
  "Create a new note linked from the current one.  Insert §ID at point.
Also add a backlink from the new note to the current one."
  (interactive)
  (unless (and buffer-file-name
               (string-prefix-p (file-truename kasten-directory)
                                (file-truename buffer-file-name)))
    (user-error "Kasten: current buffer `%s' is not a note" buffer-file-name))

  (pcase-let* ((origin-path buffer-file-name)
               (origin-id (file-name-base origin-path))
               (`(:id ,new-id :path ,new-path) (kasten--generate-id-and-path))
               (new-file (concat new-path "." kasten-default-extension))
               (new-dir (file-name-directory new-file)))
    (insert (concat kasten-id-symbol new-id))
    (unless (file-directory-p new-dir)
      (progn
	(make-directory new-dir t)
	(message "Kasten: created directory `%s'" new-dir)))
    (find-file new-file)
    (insert kasten-note-template)
    (save-excursion
      (insert "\n")
      (insert
       (concat kasten-backlink-comment kasten-id-symbol origin-id "\n")))
    (save-buffer)
    (message "Kasten: created new note `%s' backlinking to `%s' at `%s'"
	     new-id new-file origin-id)))

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
  (concat
   (if kasten-index-hidden-files "\\." "^[^.].*\\.")
   (regexp-opt kasten-file-extensions t) "$"))

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
      (message "Kasten: could not open file with ID `%s': not found"
	       filename))))

(defun kasten--id-to-file (id)
  "Find full path of note file matching ID."
  (seq-find (lambda (f)
	      (string= (file-name-base f) id))
            (kasten--get-note-files)))

(defun kasten--follow-id-link (id)
  "Open the file corresponding to ID."
  (interactive)
  (let ((file (kasten--id-to-file id)))
    (if file
        (find-file file)
      (message "Kasten: could not follow ID `%s': file not found" id))))

(defun kasten-follow-id-link-at-point ()
  "Follow the kasten ID link at point by calling `kasten--follow-id-link'."
  (interactive)
  (let ((id (thing-at-point 'symbol t)))
    (if id
        (kasten--follow-id-link id)
      (debug))))

(defcustom kasten-follow-id-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'kasten-follow-id-link-at-point)
    (define-key map (kbd "s-RET") #'kasten-follow-id-link-at-point)
    map)
  "Keymap used on Kasten ID links."
  :type 'keymap
  :group 'kasten)

(defun kasten--fontify-clickable-id ()
  "Make Kasten ID clickable in buffers."
  (font-lock-add-keywords
   nil
   `((,kasten-id-regexp
      (0 (progn
           (add-text-properties
	    (match-beginning 0) (match-end 0)
	    `(mouse-face highlight
                         help-echo "\\{kasten-follow-id-keymap}"
                         face org-link
                         keymap ,kasten-follow-id-keymap)))
         nil))))
  (font-lock-flush))

(add-hook 'org-mode-hook #'kasten--fontify-clickable-id)

(defun kasten-insert-id ()
  "Prompt to insert an ID referencing a note."
  (interactive)
  (let* ((files (kasten--get-note-files))
         (ids (mapcar #'file-name-base files))
         (id (completing-read
	      (concat "[Kasten] Insert ID: " kasten-id-symbol) ids nil nil)))
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
         (tag (completing-read
	       "[Kasten] Search tag: " tags nil nil kasten-tag-symbol)))
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
         (tag (completing-read
	       "[Kasten] Insert tag: " tags nil nil kasten-tag-symbol)))
    (insert tag)))

(defun kasten-show-backlinks-current-note ()
  "Show all notes that link to the current note by ID."
  (interactive)
  (let* ((file (buffer-file-name))
         (id (and file (file-name-base file))))
    (if (not id)
        (message "Kasten: not visiting a note file")
      (funcall kasten-search-function
	       kasten-directory (concat kasten-id-symbol (regexp-quote id))))))

(defun kasten-show-backlinks (id)
  "Prompt for an ID and show backlinks to it across the notes."
  (interactive
   (let* ((ids (mapcar #'file-name-base (kasten--get-note-files)))
          (choice (completing-read
		   ("[Kasten] Show backlinks to ID: " kasten-id-symbol)
		   ids nil nil)))
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
