;;; kasten.el --- Zettelkasten note browser  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiyang Sun

;; Author: Qiyang Sun <iamsqy@outlook.com>
;; Maintainer: Qiyang Sun <iamsqy@outlook.com>
;; Created: 21 Jun 2025
;; URL: https://github.com/iamsqy/kasten
;; Package-Version: 0.2
;; Package-Requires: ((emacs "27.1") (consult "0.33"))
;; Keywords: tools, files, matching

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
(require 'button)

(defgroup kasten nil
  "Zettelkasten note browsing tool."
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

(defcustom kasten-note-title-template "#+title: "
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
  "Max position of the title of a note.
Depth for Kasten to match `kasten-title-regexp'."
  :type 'integer
  :group 'kasten)

(defcustom kasten-category-regexp
  "^#\\+[cC][aA][tT][eE][gG][oO][rR][yY]: *\\(.*\\)$"
  "Regexp to match the category of a note."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-empty-category-placeholder ""
  "If non-empty, display in category field for uncategorised notes.
Leave empty to omit empty category for uncategorised notes."
  :type 'string
  :group 'kasten)

(defcustom kasten-category-max-pos 4096
  "Max position of the category of a note.
Depth for Kasten to match `kasten-category-regexp'."
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
  "Max position of the title of a note.
Depth for Kasten to match `kasten-tag-regexp'."
  :type 'integer
  :group 'kasten)

(defcustom kasten-tag-symbol "#"
  "Leading symbol for tag."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-tag-first-char-regexp "\\x23"
  "Regexp for the tag char to make `kasten-search-function' understand.

If using `consult-ripgrep', use `\\x23' instead of `\#'."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-id-symbol "§"
  "Leading symbol for ID."
  :type 'string
  :group 'kasten)

(defcustom kasten-id-format "%y%m%d-%H%M"
  "Time format for generating ID.  Use `%E' for shortened title.
See man page `strftime(3)' for wildcards."
  :type 'string
  :group 'kasten)

(defcustom kasten-folder-timeformat "%Y%m"
  "Time format for folder.
See man page `strftime(3)' for wildcards."
  :type 'string
  :group 'kasten)

(defcustom kasten-id-clash-time-inc 60
  "Time interval to increment if the time-based ID clashes, in seconds.
Set to 60 for 1 minute."
  :type 'integer
  :group 'kasten)

(defcustom kasten-id-regexp "§\\([0-9]\\{6\\}-[0-9]\\{4\\}\\)"
  "Regexp for an ID in a note."
  :type 'regexp
  :group 'kasten)

(defcustom kasten-backlink-comment "#+backlink: "
  "Insert before backlink ID when creating new note at point."
  :type 'string
  :group 'kasten)

(defcustom kasten-buffer-title "Kasten"
  "Kasten mode buffer title."
  :type 'string
  :group 'kasten)

(defcustom kasten-minor-mode-lighter " Kt"
  "Kasten minor mode lighter."
  :type 'string
  :group 'kasten)

(defface kasten-buffer-title-face
  '((t :family "variable-pitch" :weight bold :height 2.33
       :foreground "cyan" :background "navy" :slant italic
       :underline (:color "blue" :style line) :extend t))
  "Face for the Kasten buffer title."
  :group 'kasten)

(defface kasten-file-title-face
  '((t :foreground "gold" :weight bold))
  "Face for file titles in Kasten."
  :group 'kasten)

(defface kasten-title-category-split-face
  '((t :foreground "gray"))
  "Face for title category split in Kasten."
  :group 'kasten)

(defface kasten-file-category-face
  '((t :foreground "purple"))
  "Face for categories in Kasten."
  :group 'kasten)

(defface kasten-file-name-face
  '((t :foreground "green"))
  "Face for filenames in Kasten."
  :group 'kasten)

(defface kasten-live-search-edit-face
  '((t :foreground "white" :background "dim gray" :box (:line-width 1)))
  "Face for categories in Kasten."
  :group 'kasten)

(defface kasten-button-face
  '((t :foreground "black" :background "gray"
       :box (:line-width 3 :style released-button)
       :family "variable-pitch" :height 0.67))
  "Face for categories in Kasten."
  :group 'kasten)

(defvar kasten-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'kasten-open-file)
    (define-key map (kbd "/") #'kasten-live-search)
    (define-key map (kbd "?") #'kasten-filters-edit)
    (define-key map (kbd "g") #'kasten-refresh)
    (define-key map (kbd "s") #'kasten-search)
    (define-key map (kbd "t") #'kasten-search-tag)
    (define-key map (kbd "n") #'kasten-create-new-note)
    (define-key map (kbd "DEL") #'delete-backward-char)
    map)
  "Keymap for Kasten mode.")

(defvar kasten-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k k") #'kasten)
    (define-key map (kbd "C-c C-k b") #'kasten-show-backlinks-current-note)
    (define-key map (kbd "C-c C-k i") #'kasten-insert-id)
    (define-key map (kbd "C-c C-k I") #'kasten-change-id)
    (define-key map (kbd "C-c C-k n") #'kasten-create-new-note-at-point)
    (define-key map (kbd "C-c C-k t") #'kasten-insert-tag)
    (define-key map (kbd "C-c C-k T") #'kasten-search-tag)
    map)
  "Keymap for Kasten minor mode.")

(define-derived-mode kasten-mode special-mode "Kasten"
  "Major mode for browsing notes."
  (setq buffer-read-only t)
  (hl-line-mode 1)
  (when kasten--is-live-search
    (kasten-live-search))
  (when kasten-auto-refresh
    (kasten--enable-auto-refresh))
  (kasten-refresh t t))

(define-minor-mode kasten-minor-mode
  "Minor mode for extra Kasten features in note buffers."
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
:mode is either `or' or `and' to combine the title and category filters.")

(defun kasten-filters-active-p ()
  "Return non-nil if `kasten-filters' has active filters."
  (let ((titles (plist-get kasten-filters :title))
        (categories (plist-get kasten-filters :category)))
    (or titles categories)))

(defun kasten--matches-filter-p (title category)
  "Return t if TITLE and CATEGORY pass the filters in `kasten-filters'."
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
  "Keymap for Kasten filters mode'.")

(defvar kasten-filters-font-lock-keywords
  '(("^%.*$" . font-lock-comment-face)
    ("^#.*$" . font-lock-keyword-face))
  "Font lock keywords for Kasten filters mode.")

(define-derived-mode kasten-filters-mode text-mode "Kasten-Filters"
  "Major mode for editing Kasten filters."
  (setq buffer-read-only nil)
  (setq font-lock-defaults '(kasten-filters-font-lock-keywords))
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[%#].*$" nil t)
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(read-only t front-sticky t rear-nonsticky t)))))
  (setq-local header-line-format (substitute-command-keys "[Kasten] Edit \
filters.  \\[kasten-filters-save-and-kill] to apply.  \
\\[kill-buffer-and-window] to discard.")))

(add-hook 'kasten-filters-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq truncate-lines t)))

(defun kasten-filters--insert-current ()
  "Insert the current Kasten filter in the editing buffer."
  (let ((title-list (plist-get kasten-filters :title))
        (category-list (plist-get kasten-filters :category))
        (mode (plist-get kasten-filters :mode)))
    (insert "\
% To customise Kasten filters, edit this buffer.  When done, `C-c C-c' to\n")
    (insert "\
% apply, or `C-c C-k' to discard.  Lines starting with `\%' are comments.\n")
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
  "Open a buffer for editing the Kasten filters."
  (interactive)
  (let ((buf (get-buffer-create kasten-filters-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (kasten-filters--insert-current)
      (kasten-filters-mode))
    (pop-to-buffer buf)))

(defun kasten-filters--toggle-the-mode ()
  "Toggle the mode on the current line between `or' and `and'."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (trimmed (and line (string-trim line))))
    (when (and trimmed (member trimmed '("or" "and")))
      (let ((new-mode (if (string= trimmed "or") "and" "or")))
        (delete-region (line-beginning-position) (line-end-position))
        (insert new-mode)))))

(defun kasten-filters-save-and-kill ()
  "Parse and save buffer contents for kasten filters, then kill buffer."
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
      (user-error "Kasten: `Mode' must be `'or' or `'and', got `%s'" mode))
    (setq kasten-filters
	  `(:title ,title-list :category ,category-list :mode ,mode))
    (kasten-refresh nil nil)
    (kill-buffer-and-window)))

(defvar kasten-search-term ""
  "Quick search term.")

(defvar kasten--is-live-search nil
  "Non-nil if currently doing a live search.")

(defun kasten-live-search ()
  "Live search notes by title, category or ID."
  (interactive)
  (when kasten--is-live-search
    (user-error "Kasten: already performing a live search"))
  (setq kasten--is-live-search t)
  (kasten-refresh nil nil)
  (when (< (line-number-at-pos) 3)
    (goto-char (point-min))
    (forward-line 2))
  (let ((inhibit-read-only t)
	(original-map (current-local-map))
	(map (make-keymap))
	(i 0))
    (set-keymap-parent map (current-local-map))
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
                          'kasten--live-search-inc)
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'kasten--live-search-inc)
      (setq i (1+ i)))
    (define-key map (kbd "C-g")
		(lambda ()
		  (interactive)
		  (message "Kasten: quit live search")
		  (use-local-map original-map)
		  (setq kasten--is-live-search nil)
		  (setq kasten-search-term "")
		  (kasten-refresh nil nil)
		  (setq buffer-read-only t)))
    (define-key map (kbd "ESC ESC ESC")
		(lambda ()
		  (interactive)
		  (message "Kasten: quit live search")
		  (use-local-map original-map)
		  (setq kasten--is-live-search nil)
		  (setq kasten-search-term "")
		  (kasten-refresh nil nil)
		  (setq buffer-read-only t)))
    (use-local-map map)
    (setq buffer-read-only nil)
    (message "\
Type anywhere to search titles, categories and IDs.  C-g to quit.")))

(defun kasten--matches-search-term-p (title category filename)
  "Return t if TITLE or CATEGORY or FILENAME pass search term."
  (if (string= kasten-search-term "")
      t
    (progn
      (let ((term (downcase kasten-search-term)))
	(or (string-match-p (regexp-quote term) (downcase title))
            (and
	     category (string-match-p (regexp-quote term) (downcase category)))
            (string-match-p (regexp-quote term) (downcase filename)))))))

(defun kasten--live-search-inc ()
  "Filter visible notes as user types."
  (interactive)
  (let ((char last-command-event))
    (cond
     ((or (eq char ?\d) (= char 127))
      (when (> (length kasten-search-term) 0)
        (setq kasten-search-term (substring kasten-search-term 0 -1))))
     ((= char ?\S-\ )
      (setq char ?\s))
     (t
      (setq kasten-search-term
	    (concat kasten-search-term (char-to-string char)))))
    (kasten-refresh nil nil)))

(defun kasten-refresh (&optional is-init is-auto)
  "Refresh note list.
Reset point if IS-INIT is non-nil; display message with time lapsed and
according to IS-AUTO."
  (interactive)
  (let ((buffer (get-buffer-create "*Kasten*"))
	(start-time (float-time)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
	    (saved-point (point))
	    (files (kasten--get-note-files)))
	(erase-buffer)
	(insert (propertize kasten-buffer-title
			    'face 'kasten-buffer-title-face
			    'read-only t))
	(when (or
	       (kasten-filters-active-p)
	       kasten--is-live-search)
	  (progn
	    (insert (propertize " ("
				'face 'kasten-buffer-title-face
				'read-only t))
	    (when (kasten-filters-active-p)
	      (insert (propertize "f"
				  'face 'kasten-buffer-title-face
				  'read-only t
				  'help-echo "f: filters activated")))
	    (when kasten--is-live-search
	      (insert (propertize "s"
				  'face 'kasten-buffer-title-face
				  'read-only t
				  'help-echo "s: performing a live search")))
	    (insert (propertize ")"
				'face 'kasten-buffer-title-face
				'read-only t))))
	(insert (propertize "\n"
			    'face 'kasten-buffer-title-face
			    'read-only t))
	(if kasten--is-live-search
	    (progn
	      (delete-region (line-beginning-position) (line-end-position))
	      (insert (propertize "Live search: "
				  'face 'shadow
				  'read-only t))
	      (insert (propertize (if (> (length kasten-search-term)
					 (- (window-width) 14))
				      kasten-search-term
				    (format (format "%%-%ds"
						    (- (window-width) 14))
					    kasten-search-term))
				  'face 'kasten-live-search-edit-face
				  'read-only t))
	      (insert "\n"))
	  (progn
	    (delete-region (line-beginning-position) (line-end-position))
	    (insert-button
	     (substitute-command-keys "Live Search (\\[kasten-live-search])")
	     'face 'kasten-button-face
	     'action (lambda (_button) (kasten-live-search)))
	    (insert " ")
	    (insert-button
	     (substitute-command-keys "Filters... (\\[kasten-filters-edit])")
	     'face 'kasten-button-face
	     'action (lambda (_button) (kasten-filters-edit)))
	    (insert " ")
	    (insert-button
	     (substitute-command-keys "Full Search... (\\[kasten-search])")
	     'face 'kasten-button-face
	     'action (lambda (_button) (kasten-search)))
	    (insert " ")
	    (insert-button
	     (substitute-command-keys "Search Tag... (\\[kasten-search-tag])")
	     'face 'kasten-button-face
	     'action (lambda (_button) (kasten-search-tag)))
	    (insert " ")
	    (insert-button
	     (substitute-command-keys "New Note... (\\[kasten-create-new-note])")
	     'face 'kasten-button-face
	     'action (lambda (_button) (kasten-create-new-note)))
	    (insert " ")
	    (insert-button
	     (substitute-command-keys "Refresh (\\[kasten-refresh])")
	     'face 'kasten-button-face
	     'action (lambda (_button) (kasten-refresh nil nil)))
	    (insert " ")
	    (insert-button
	     (substitute-command-keys "Quit Kasten (\\[quit-window])")
	     'face 'kasten-button-face
	     'action (lambda (_button) (quit-window)))
	    (insert "\n")))
	(dolist (file files)
	  (let* ((title (kasten--parse-org-title file))
		 (category (kasten--parse-category file))
		 (filename (file-name-base file)))
	    (when (and
		   (kasten--matches-filter-p title category)
		   (kasten--matches-search-term-p title category filename))
	      (insert (propertize title
				  'face 'kasten-file-title-face
				  'read-only t))
	      (unless (string= category "")
		(insert (propertize kasten-title-category-split
				    'face 'kasten-title-category-split-face
				    'read-only t))
		(insert (propertize category
				    'face 'kasten-file-category-face
				    'read-only t)))
	      (insert (propertize
		       (make-string ; align the line so that ID is flushed right
			(max 2 (-
				(window-width)
				(length title)
				(length filename)
				(if (string= category "") ; handle category
				    0
				  (+ (length category)
				     (length kasten-title-category-split)))
				(if (display-graphic-p) ; one char less for TTY
				    0 1)))
			?\s)
		       'read-only t))
	      (insert (propertize filename
				  'face 'kasten-file-name-face
				  'read-only t))
	      (insert (propertize "\n" 'read-only t)))))
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

(defvar kasten--resize-timer nil
  "Timer to debounce refresh after window resize.")

(defun kasten--debounced-refresh (_frame)
  "Debounce refresh by delaying it after window resize events."
  (when kasten--resize-timer
    (cancel-timer kasten--resize-timer))
  (setq kasten--resize-timer
        (run-with-idle-timer
         1 nil ; delay 1 second
         (lambda ()
           (when (derived-mode-p 'kasten-mode)
             (kasten-refresh))))))

(add-hook 'window-size-change-functions #'kasten--debounced-refresh)

(defvar kasten--watch-handle nil
  "Handle returned by `file-notify-add-watch'.")

(defun kasten--set-auto-refresh (symbol value)
  "Handle change to `kasten-auto-refresh', set SYMBOL to VALUE."
  (set-default symbol value)
  (if (eq value t)
      (kasten--enable-auto-refresh)
    (kasten--disable-auto-refresh)))

(defun kasten--maybe-auto-refresh (_event)
  "Triggers `kasten-refresh' if `kasten-auto-refresh' is non-nil."
  (when kasten-auto-refresh
    (kasten-refresh nil t)))

(defun kasten--enable-auto-refresh ()
  "Enable auto refresh when files change in `kasten-directory'."
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

(defun kasten--safetitle (title)
  "Convert TITLE to a safe short string can be used for ID or filename."
  (let* ((clean (replace-regexp-in-string "[^a-zA-Z0-9-_+]" "" title)))
    (substring clean 0 (min 8 (length clean)))))

(defun kasten--generate-id-and-path (&optional title)
  "Generate ID and path, optionally using TITLE, increment time if clash."
  (let ((n 0)
        id path)
    (cl-loop
     with now = (current-time)
     for offset-time = (time-add
			now (seconds-to-time (* kasten-id-clash-time-inc n)))
     for id-timeformat = (replace-regexp-in-string "%E"
						   (kasten--safetitle title)
						   kasten-id-format)
     for base = (format-time-string id-timeformat offset-time)
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
  (pcase-let* ((title (read-from-minibuffer "[Kasten] Title for new note: "))
	       (`(:id ,id :path ,path) (kasten--generate-id-and-path title))
               (full-path (concat path "." kasten-default-extension))
               (dir (file-name-directory full-path)))
    (unless (file-directory-p dir)
      (progn
	(make-directory dir t)
	(message "Kasten: created directory `%s'" dir)))
    (find-file full-path)
    (insert kasten-note-title-template)
    (insert title)
    (save-buffer)
    (message "Kasten: created new note `%s' at `%s'" id full-path)))

(defun kasten-create-new-note-at-point ()
  "Create a new note linked from the current one.

Insert ID at point and add a backlink from the new note to the current one."
  (interactive)
  (unless (and buffer-file-name
               (string-prefix-p (file-truename kasten-directory)
                                (file-truename buffer-file-name)))
    (user-error "Kasten: current buffer `%s' is not a note" buffer-file-name))

  (pcase-let* ((origin-path buffer-file-name)
               (origin-id (file-name-base origin-path))
               (title (read-from-minibuffer "[Kasten] Title for new note: "))
	       (`(:id ,id :path ,path) (kasten--generate-id-and-path title))
               (new-file (concat new-path "." kasten-default-extension))
               (new-dir (file-name-directory new-file)))
    (insert (concat kasten-id-symbol new-id))
    (unless (file-directory-p new-dir)
      (progn
	(make-directory new-dir t)
	(message "Kasten: created directory `%s'" new-dir)))
    (find-file new-file)
    (insert kasten-note-title-template)
    (save-excursion
      (insert "\n")
      (insert
       (concat kasten-backlink-comment kasten-id-symbol origin-id "\n")))
    (save-buffer)
    (message "Kasten: created new note `%s' backlinking to `%s' at `%s'"
	     new-id new-file origin-id)))

(defun kasten--parse-org-title (file)
  "Return the title from FILE or fallback to base filename."
  (with-temp-buffer
    (insert-file-contents file nil 0 kasten-title-max-pos)
    (let ((contents (buffer-string)))
      (if (string-match kasten-title-regexp contents)
          (let ((title (string-trim (match-string 1 contents))))
	    (if (string= title "")
		(concat "<untitled> " (file-relative-name file))
	      title))
        (file-name-base file)))))

(defun kasten--parse-category (file)
  "Return the category of FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 kasten-category-max-pos)
    (let ((contents (buffer-string)))
      (if (string-match kasten-category-regexp contents)
          (string-trim (match-string 1 contents))
        kasten-empty-category-placeholder))))

(defun kasten--extension-regexp ()
  "Return a regexp matching extensions in `kasten-file-extensions'."
  (concat
   (if kasten-index-hidden-files "\\." "^[^.].*\\.")
   (regexp-opt kasten-file-extensions t) "$"))

(defun kasten--get-note-files ()
  "Return a list of note file paths in `kasten-directory'."
  (directory-files-recursively kasten-directory (kasten--extension-regexp)))

(defun kasten-open-file ()
  "Open the note file on the current line by matching filename."
  (interactive)
  (unless (> (line-number-at-pos) 2)
    (user-error "Kasten: not on a note, cannot open"))
  (let* ((line (thing-at-point 'line t))
         (id (and line (string-trim (car (last (split-string line))))))
         (file (kasten--id-to-file id)))
    (if file
	(progn
          (find-file file)
	  (kasten-minor-mode 1)
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
    (when id
        (kasten--follow-id-link id))))

(defun kasten--button-action (button)
  "Action to perform when BUTTON is clicked."
  (let ((id (button-get button 'kasten-id)))
    (kasten--follow-id-link id)))

(defun kasten--add-id-buttons ()
  "Add Kasten ID buttons in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward kasten-id-regexp nil t)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (id (string-remove-prefix kasten-id-symbol (match-string 0))))
        (unless (get-text-property start 'button)
          (make-button
           start end
           'action #'kasten--button-action
           'kasten-id id
           'face 'org-link
           'follow-link t))))))

(defun kasten--setup-id-buttons ()
  "Setup Kasten ID buttons when opening notes."
  (add-hook 'after-change-functions #'kasten--after-change-update-buttons nil t)
  (kasten--add-id-buttons))

(defun kasten--after-change-update-buttons (_beg _end _len)
  "Refresh Kasten ID buttons on text change."
  (remove-overlays)
  (kasten--add-id-buttons))

(add-hook 'org-mode-hook #'kasten--setup-id-buttons)

(defun kasten-insert-id ()
  "Prompt to insert an ID referencing a note."
  (interactive)
  (let* ((files (kasten--get-note-files))
         (ids (mapcar #'file-name-base files))
         (id (completing-read
	      (concat "[Kasten] Insert ID: " kasten-id-symbol) ids nil nil)))
    (when (and id (not (string-empty-p id)))
      (insert (concat kasten-id-symbol id)))))

(defun kasten-change-id (old-id)
  "Change occurrences of OLD-ID to a new ID in all note files.

For each file that contains OLD-ID, ask whether to replace it."
  (interactive
   (let* ((files (kasten--get-note-files))
          (ids (mapcar #'file-name-base files)))
     (list (completing-read
	    (concat "[Kasten] Change which ID: " kasten-id-symbol)
	    ids nil nil))))
  (let* ((files (kasten--get-note-files))
        (ids (mapcar #'file-name-base files))
	(new-id (read-string
		 (format (concat "[Kasten] Change `" kasten-id-symbol
				 "%s' to: " kasten-id-symbol)
			 old-id)))
	(start-time (float-time))
	(modified-file-cnt 0)
	(modification-cnt 0))
    (when (member new-id ids)
      (user-error "Kasten: new ID already exists"))
    (with-temp-message
	(format "Kasten: checking for references to `%s', may take some time..."
		old-id)
      (dolist (file (kasten--get-note-files))
	(with-temp-buffer
          (insert-file-contents file)
          (when (search-forward old-id nil t)
            (goto-char (point-min))
            (when (y-or-n-p (format "Kasten: change `%s' in `%s' to `%s'?"
                                    old-id (file-name-nondirectory file) new-id))
	      (progn
		(setq modified-file-cnt (+ modified-file-cnt 1))
		(while (search-forward old-id nil t)
		  (progn
		    (replace-match new-id)
		    (setq modification-cnt (+ modification-cnt 1)))
		  (write-region (point-min) (point-max) file))))))))
    (let* ((old-file (kasten--id-to-file old-id))
	   (old-file-relative-name (file-relative-name old-file))
	   (old-file-path (file-name-directory old-file))
	   (new-file-name (concat new-id
				  "."
				  (file-name-extension
				   (kasten--id-to-file old-id))))
	   (elapsed (- (float-time) start-time)))
      (rename-file old-file (concat old-file-path new-file-name))
      (message
       (format "Kasten: `%s%s' changed to `%s%s', with %d substitutions \
across %d files; moved file `%s' to `%s'; took %.6f seconds \
(including waiting time)"
	       kasten-id-symbol old-id
	       kasten-id-symbol new-id
	       modification-cnt modified-file-cnt
	       old-file-relative-name
	       (file-relative-name (kasten--id-to-file new-id))
	       elapsed)))))

(defun kasten-search ()
  "Search using `kasten-search-function'."
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

(defun kasten-get-note-path (id)
  "Return the path of the note corresponding to ID.
If called interactively and ID is not provided, use buffer filename."
  (interactive
   (list
    (let* ((file (buffer-file-name))
           (id (when file
                 (file-name-base file))))
      (unless id
        (user-error "Kasten: buffer is not visiting a file, cannot derive ID"))
      id)))
  (let ((note-file (kasten--id-to-file id)))
    (if note-file
        (let ((full-path (expand-file-name note-file)))
          (when (called-interactively-p 'interactive)
            (message "Kasten: note path is `%s'" full-path))
          full-path)
      (user-error "Kasten: given ID does not correspond to a note"))))

(defun kasten-get-attachment-path (id &optional create-if-nonexist subdir)
  "Return the attachment path of the note corresponding to ID.
If CREATE-IF-NONEXIST is non-nil and the attachment path does not exist,
make the directory.  If SUBDIR is non-nil, append SUBDIR to the attachment
path.  If called interactively and ID is not provided, use buffer filename."
  (interactive
   (list
    (let* ((file (buffer-file-name))
           (id (when file
                 (file-name-base file))))
      (unless id
        (user-error "Kasten: buffer is not visiting a file, cannot derive ID"))
      id)))
  (let ((note-file (kasten--id-to-file id)))
    (if note-file
        (let* ((note-dir (file-name-directory note-file))
	      (name-no-ext (file-name-base note-file))
	      (attachment-path (concat note-dir name-no-ext "/"
				       (when subdir
					 (concat subdir "/")))))
	  (when (and create-if-nonexist
		     (not (file-directory-p attachment-path)))
	    (progn
	      (make-directory attachment-path t)
	      (message "Kasten: created attachment directory `%s'"
		       attachment-path)))
          (when (called-interactively-p 'interactive)
            (message "Kasten: note attachment path is `%s'" attachment-path))
          attachment-path)
      (user-error "Kasten: given ID does not correspond to a note"))))

;;;###autoload
(defun kasten ()
  "Launch Kasten major mode."
  (interactive)
  (switch-to-buffer "*Kasten*")
  (kasten-mode))

(provide 'kasten)

;;; kasten.el ends here
