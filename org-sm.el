;;; org-ov-highlighter.el --- Highlight text in org-mode with overlays  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/scimax/org-ov-highlighter.el

;; Version: 0.1.0
;; Keywords: org-mode, highlight
;; Package-Requires: ((hydra "0.13.2") (dash) (s))
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; org-ov-highlighter provides a lightweight way to highlight text and put notes on
;; them in org-mode.

;; There is a hydra menu to make accessing all the commands below convenient:
;; `org-ov-highlighter/body'. I suggest you bind it to a key like H-h.

;; You can select text, and run these commands to add highlighting to it:
;; `org-ov-highlight' will prompt you for a color, and highlight with it.
;; These convenience functions skip the color prompt.
;; `org-ov-highlight-yellow'
;; `org-ov-highlight-blue'
;; `org-ov-highlight-green'
;; `org-ov-highlight-pink'

;; `org-ov-highlight-toggle-mouse-highlight' makes it possible to highlight text in
;; green using the mouse. Use a prefix arg to select the color.

;; `org-ov-highlight-note' will prompt you for a color, highlight the text and add
;; a Note to it as a tooltip. Notes are still experimental. You can edit a note
;; with `org-ov-highlight-note-edit'.

;; You can list all the highlights with `org-ov-highlight-list' in a buffer that
;; you can easily navigate to each one from.

;; You can clear a single highlight with `org-ov-highlight-clear'.
;; Remove all the highlights with `org-ov-highlight-clear-all'.

;; org-ov-highlighter uses a local save-buffer-hook to update the data when you
;; save the buffer. It also uses local file variables to load the highlights
;; when you open the file.

;; Known issues:

;; - Highlights do not export in org-mode. They are not part of org-syntax, so
;; you would have to use a preprocessing hook function to make it work. 

;; Highlights are not visible everywhere. So far they don't seem to work in:
;; - tables, or code-blocks.
;; - equations
;; - probably other things that are fontified by org-mode.
;; - Highlights don't seem to copy and paste. This is related to the text
;;   properties I think. I am not sure how to fix it.

(require 'hydra)
(require 'ov)

;;; Code:
(defgroup org-ov-highlighter nil
  "Customization group for `org-ov-highlighter'."
  :tag "org-ov-highlighter")


(defcustom org-ov-highlight-mouse-color "Darkolivegreen1"
  "Color to use for mouse highlighting."
  :type 'string
  :group 'org-ov-highlighter)


;; * Highlight text and functions

(defun org-ov-highlight-color-chooser ()
  "Interactively choose a color with completion."
  (plist-get (get-text-property
	      0 'face
	      (completing-read
	       "Color: "
	       (progn
		 (save-selected-window
		   (list-colors-display))
		 (prog1
		     (with-current-buffer (get-buffer "*Colors*")
		       (mapcar (lambda (line)
				 (append (list line) (s-split " " line t)))
			       (s-split "\n" (buffer-string))))
		   (kill-buffer "*Colors*")))))
	     :background))


;;;###autoload
(defun org-ov-highlight (beg end &optional color)
  "Highlight region from BEG to END with COLOR.
COLOR is selected from `org-ov-highlight-color-chooser' when run interactively."
  (interactive "r")
  (unless color
    (setq color (org-ov-highlight-color-chooser)))

  ;; add a local hook
  (add-hook 'after-save-hook 'org-ov-highlight-save nil t)
  
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face `(:background ,color))
    (overlay-put ov 'org-ov-highlight t)
    (set-buffer-modified-p t)
    ov))


(defun org-ov-get-highlight-overlays ()
  "Return a list of the highlight overlays.
The list is from first to last."
  (reverse (-filter (lambda (ov) (overlay-get ov 'org-ov-highlight)) 
		    (overlays-in (point-min) (point-max)))))

;;;###autoload
(defun org-ov-highlight-list ()
  "Make a list of highlighted text in another buffer."
  (interactive)
  (let* ((links (mapcar
		 (lambda (entry)
		   (format "[[elisp:(progn (find-file-other-window \"%s\")(goto-char %s))][link]] %-40s|%s\n"
			   (buffer-file-name)
			   (nth 0 entry)
			   (propertize
			    (buffer-substring (nth 0 entry) (nth 1 entry))
			    'font-lock-face `(:background ,(nth 2 entry)))
			   (nth 3 entry)))
		 (org-ov-highlight-get-highlights))))
    (if links
	(progn
	  (when (= (length (window-list)) 1)
	    (split-window-horizontally))
	  (switch-to-buffer-other-window "*highlights*") (org-mode)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert "Click on link to jump to the position. Press q to quit.\n\n")

	  (dolist (link links)
	    (insert link))
	  (use-local-map (copy-keymap org-mode-map))
	  (local-set-key "q"
			 #'(lambda ()
			     (interactive)
			     (delete-window)))
	  (setq buffer-read-only t))
      (message "No highlights found."))))


;;;###autoload
(defun org-ov-highlight-yellow ()
  "Highlight region in yellow."
  (interactive)
  (org-ov-highlight (region-beginning) (region-end) "Yellow"))


;;;###autoload
(defun org-ov-highlight-blue ()
  "Highlight region in blue."
  (interactive)
  (org-ov-highlight (region-beginning) (region-end) "Blue"))


;;;###autoload
(defun org-ov-highlight-pink ()
  "Highlight region in pink."
  (interactive)
  (org-ov-highlight (region-beginning) (region-end) "Pink"))


;;;###autoload
(defun org-ov-highlight-green ()
  "Highlight region in green."
  (interactive)
  (org-ov-highlight (region-beginning) (region-end) "Darkolivegreen1"))


(defvar org-ov-highlight-mouse nil
  "Stores if highlight mouse mode is active.")


;; create the advice for use later
(defadvice mouse-set-region (after org-ov-highlight () disable)
  "Advice for mouse highlighting."
  (when (eq major-mode 'org-mode)
    (org-ov-highlight (region-beginning) (region-end)
		      org-ov-highlight-mouse-color)))


;;;###autoload
(defun org-ov-highlight-toggle-mouse-highlight (arg)
  "Toggle mouse highlighting.
The default color is `org-ov-highlight-mouse-color'. Use a prefix
ARG to select a different color and save it."
  (interactive "P")
  (when arg
    (setq org-ov-highlight-mouse-color (org-ov-highlight-color-chooser)))
  
  (if org-ov-highlight-mouse
      ;; Turn it off
      (progn (ad-disable-advice 'mouse-set-region 'after 'org-ov-highlight)
	     (ad-deactivate 'mouse-set-region)
	     (setq org-ov-highlight-mouse nil)
	     (message "Mouse highlighting off."))
    (ad-enable-advice 'mouse-set-region 'after 'org-ov-highlight)
    (ad-activate 'mouse-set-region)
    (setq org-ov-highlight-mouse t)
    (message "Mouse highlighting on.")))


;;;###autoload
(defun org-ov-highlight-note (beg end &optional color note)
  "Highlight selected text from BEG to END with COLOR.
Add NOTE to it as a tooltip. If no text is selected, insert \" note \"
and propertize it."
  (interactive "r")
  (add-hook 'after-save-hook 'org-ov-highlight-save nil t)

  (unless color (setq color (org-ov-highlight-color-chooser)))
  (unless note (setq note (read-input "Note: ")))

  (let ((ov (org-ov-highlight beg end color)))
    (overlay-put ov 'help-echo note)
    (set-buffer-modified-p t)))


;;;###autoload
(defun org-ov-highlight-note-edit (new-note)
  "Set tooltip of highlight at point to NEW-NOTE."
  (interactive (list (read-input
		      "Note: "
		      (overlay-get (ov-at) 'help-echo))))
  (overlay-put (ov-at) 
	       'help-echo new-note))


;;;###autoload
(defun org-ov-highlight-clear ()
  "Clear highlight at point."
  (interactive)
  (delete-overlay (ov-at))
  (set-buffer-modified-p t))


;;;###autoload
(defun org-ov-highlight-clear-all ()
  "Clear all highlighted text.
They are really deleted when you save the buffer."
  (interactive)
  (mapc 'delete-overlay (org-ov-get-highlight-overlays)) 
  (when (get-buffer "*highlights*")
    (kill-buffer "*highlights*"))
  (set-buffer-modified-p t)
  (save-buffer))


;;;###autoload
(defhydra org-ov-highlighter (:color blue) "highlighter"
  ("b" org-ov-highlight-blue "blue")
  ("g" org-ov-highlight-green "Green")
  ("p" org-ov-highlight-pink "Pink")
  ;; define as many special colors as you like.
  ("s" (org-ov-highlight (region-beginning) (region-end) "Lightsalmon1") "Salmon")
  ("y" org-ov-highlight-yellow "yellow")
  ("c" org-ov-highlight "Choose color")
  ("n" (org-ov-highlight-note (region-beginning) (region-end) "Thistle") "Note")
  ("N" org-ov-highlight-note "Note (c)")
  ("m" org-ov-highlight-toggle-mouse-highlight "Toggle mouse")
  ("e" org-ov-highlight-note-edit "Edit note")

  ;; Grading/feedback options
  ("t" org-ov-highlight-typo "Typo")
  ("f" org-ov-highlight-feedback "Feedback note")
  
  ("l" org-ov-highlight-list "List highlights")
  ("d" org-ov-highlight-clear "Delete")
  ("D" org-ov-highlight-clear-all "Delete All"))


(defun org-ov-highlighter-menu ()
  "Add org-ov-highlighter to the Org menu."
  (easy-menu-change
   '("Org") "Highlighter"
   '(["Highlight (B)" org-ov-highlight-blue]
     ["Highlight (G)" org-ov-highlight-green]
     ["Highlight (P)" org-ov-highlight-pink]
     ["Highlight (Y)" org-ov-highlight-yellow]
     ["Highlight note" org-ov-highlight-note]
     ["List highlights" org-ov-highlight-list]
     ["Delete highlight" org-ov-highlight-clear]
     ["Delete highlights" org-ov-highlight-clear-all])
   "Show/Hide")
  (easy-menu-change '("Org") "--" nil "Show/Hide"))

(add-hook 'org-mode-hook 'org-ov-highlighter-menu)


;; * Save and load functions
(defun org-ov-highlight-get-highlights ()
  "Returns a list of (beg end color note) for the overlays."
  (mapcar (lambda (ov)
	    (list (overlay-start ov)
		  (overlay-end ov)
		  (plist-get  (overlay-get ov 'face) :background)
		  (overlay-get ov 'help-echo)))
	  (org-ov-get-highlight-overlays)))


(defun org-ov-highlight-save ()
  "Save highlight information.
Data is saved in an org-section in the document."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* org-ov-highlighter data" nil 'mv)
      (insert "* org-ov-highlighter data :noexport:
  :PROPERTIES:
  :VISIBILITY: folded
  :ID: org-ov-highlighter-data
  :END:\nDo not delete this section. It stores information about the highlights in this document. Any information in this section may be deleted if you remove the highlights in this document.\n#+name: org-ov-highlighter-data\n#+BEGIN_SRC emacs-lisp :results code value replace\n(org-ov-highlight-get-highlights)\n#+END_SRC")

      (add-file-local-variable 'eval '(progn (require 'org-ov-highlighter) (org-ov-highlight-load))))
    (org-save-outline-visibility nil
      (org-babel-goto-named-src-block "org-ov-highlighter-data")
      (org-babel-execute-src-block)

      ;; delete section if there are no highlights
      (when (null (org-ov-highlight-get-highlights))
	(outline-previous-heading)
	(let ((hl (org-element-context)))
	  (setf (buffer-substring (org-element-property :begin hl)
				  (org-element-property :end hl))
		"")))
      (let ((after-save-hook '()))
	(save-buffer)
	(org-set-visibility-according-to-property)))))


(defun org-ov-highlight-load ()
  "Load and apply highlighted text."
  (interactive)
  
  (org-babel-goto-named-result "org-ov-highlighter-data")
  (let ((hls (read (org-element-property :value (org-element-context)))))
    (mapc
     (lambda (entry)
       (let ((beg (nth 0 entry))
	     (end (nth 1 entry))
	     (color (nth 2 entry))
	     (help-echo (nth 3 entry)))
	 (if help-echo
	     (org-ov-highlight-note beg end color help-echo)
	   (org-ov-highlight beg end color))))
     hls))
  (add-hook 'after-save-hook 'org-ov-highlight-save nil t))

;; add the local var we use as safe so we don't get annoyed by permission to run
;; it.
(add-to-list 'safe-local-eval-forms
	     '(progn (org-ov-highlight-load)))


;; * Feedback functions
;;;###autoload
(defun org-ov-highlight-typo ()
  "Add a typo highlight."
  (interactive)
  (let* ((r1 (progn (re-search-backward "\\<") (set-mark (point)) (point)))
	 (r2 (progn (re-search-forward "\\>") (point))))
    (org-ov-highlight-note r1 r2 "PaleVioletRed1" "typo")))


;;;###autoload
(defun org-ov-highlight-feedback ()
  "Add a feedback highlight."
  (interactive)
  (let (r1 r2 comment)
    (if (region-active-p)
	(setq r1 (region-beginning)
	      r2 (region-end))
      ;; No region, so we make one
      (setq  r1 (progn (re-search-backward "\\<") (set-mark (point)) (point))
	     r2 (progn (re-search-forward "\\>") (point))))

    (setq comment (read-input "Comment: "))
    (org-ov-highlight-note r1 r2 "LightBlue1" comment)))

(provide 'org-ov-highlighter)

;;; org-ov-highlighter.el ends here

(defcustom org-sm-server-url "http://172.16.22.130:31337"
  "Host URL for SuperMemo server."
  :type 'string)

(defcustom org-sm-comment-registered-signature "org-sm-registered"
  "Signature that gets added to SM element comments to indicate they have an associated emacs entry."
  :type 'string)

(defcustom org-sm-grades '(:bad :fail :pass :good :great)
  "A sequence of grades"
  :type 'sequence)

(defun org-sm-apiclient-http-do (method url headers entity)
  "Send ENTITY and HEADERS to URL as a METHOD request.
ENTITY is a list"
  (let ((url-request-method (encode-coding-string method 'us-ascii))
        (url-request-extra-headers '())
        (url-request-data (encode-coding-string (json-encode-list entity) 'utf-8))
        (url-mime-charset-string (url-mime-charset-string))
        (url-mime-language-string nil)
        (url-mime-encoding-string nil)
        (url-mime-accept-string nil)
        (url-personal-mail-address nil))

    (dolist (header headers)
      (let* ((mapped (assoc-string (downcase (car header))
                                   '(("from" . url-personal-mail-address)
                                     ("accept-encoding" . url-mime-encoding-string)
                                     ("accept-charset" . url-mime-charset-string)
                                     ("accept-language" . url-mime-language-string)
                                     ("accept" . url-mime-accept-string)))))

        (if mapped
            (set (cdr mapped) (encode-coding-string (cdr header) 'us-ascii))
          (let* ((hkey (encode-coding-string (car header) 'us-ascii))
                 (hvalue (encode-coding-string (cdr header) 'us-ascii)))
            (setq url-request-extra-headers (cons (cons hkey hvalue) url-request-extra-headers))))))

    (setq org-sm-apiclient-within-call t)
    (setq org-sm-apiclient-request-time-start (current-time))
    (url-retrieve-synchronously url nil nil 10)))

(defun org-sm-apiclient-http-with-sm-server-url-do (method path &optional entity headers)
  "Send HEADERS and ENTITY to PATH of SuperMemo server host url as a METHOD request.
ENTITY is a list, is default empty. Headers is default '((\"Content-Type\" . \"application/json\"))"
  (org-sm-apiclient-http-do method
                             (concat org-sm-server-url "/" path)
                             (if headers headers '(("Content-Type" . "application/json")))
                             entity))

(defun org-sm-apiclient-http-with-sm-server-url-do-and-parse-current (method path &optional entity headers)
  "Send HEADERS and ENTITY to PATH of SuperMemo server host url as a METHOD request.
ENTITY is a list, is default empty. Headers is default '((\"Content-Type\" . \"application/json\"))"
  (with-current-buffer
      (org-sm-apiclient-http-with-sm-server-url-do method path entity headers)
    (save-excursion
      (goto-char (point-min))
      (search-forward "{")
      (backward-char)
      (json-parse-buffer))))

(defun org-sm-apiclient-http-ping ()
   (org-sm-apiclient-http-response-p
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "was-graded")))

(defun org-sm-apiclient-element-back ()
   (org-sm-apiclient-http-response-p
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "element-back")))

(defun org-sm-apiclient-element-forward ()
   (org-sm-apiclient-http-response-p
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "element-forward")))

(defun org-sm-apiclient-http-response-result-eq (response result)
  (equal (gethash "result" response "false") result))

(defun org-sm-apiclient-http-response-p (response)
  (org-sm-apiclient-http-response-result-eq response "true"))

(defun org-sm-apiclient-graded-p ()
   (org-sm-apiclient-http-response-p
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "was-graded")))

(defun org-sm-apiclient-set-grade (grade)
  (org-sm-apiclient-http-response-p
   (let ((gradepos (cl-position grade org-sm-grades)))
     (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current
      "POST"
      "set-grade"
      (list (cons "grade" gradepos))))))

(defun org-sm-apiclient-ready-to-grade-p ()
   (org-sm-apiclient-http-response-p
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "ready-to-grade")))

(defun org-sm-apiclient-learning-mode ()
  (let ((response (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "learning-mode")))
    (let* ((result (gethash "result" response "none")))
      (cdr (assoc-string (downcase result)
                    '(("none" . :none)
                      ("standard" . :standard)))))))

(defun org-sm-apiclient-current-repetition ()
  "Returns whether the operation was success (therefore grade is ready.)"
   (org-sm-apiclient-http-response-p
    (if (eq (org-sm-apiclient-learning-mode) :none)
        (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "begin-learning")
      (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "next-element"))))

(defun org-sm-apiclient-next-repetition ()
  "Used to go to next repetition, only to be used to move forward from "
   (org-sm-apiclient-http-response-p
    (if (org-sm-apiclient-item-p)
        (org-sm-apiclient-current-repetition)
      (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "next-repetition"))))

(defun org-sm-apiclient-element-info ()
  (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "element-info"))

(defun org-sm-apiclient-postpone (days)
  "Reschedule next interval for element DAYS number of days in the future."
  (when (and (integerp days) (< 0 days))
    (org-sm-apiclient-http-response-p
     (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "POST" "postpone"  (list (cons "days" days))))))

(defun org-sm-apiclient-set-priority (priority)
  (unless (and (isnan priority) (>= 0.0 priority) (<= 100.0 priority))
    (org-sm-apiclient-http-response-p
     (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current
      "POST"
      "set-priority"
      (list (cons "priority" priority))))))

(defun org-sm-apiclient-element-create (type)
  (org-sm-apiclient-http-response-p
   (cl-case type
      (:item
       (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "new-item"))
      (:topic
       (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "new-topic")))))

(defun org-sm-apiclient-element-extract-create ()
  (org-sm-apiclient-http-response-p
   (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "extract")))

(defun org-sm-apiclient-store-element-id (id)
  (org-sm-apiclient-http-response-p
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current
     "POST"
     "append-comment"
     (list (cons "comment" (concat org-sm-comment-registered-signature "<" (princ id) ">"))))))

(defun org-sm-apiclient-get-element-id ()
  (let ((response (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "comment")))
    (let* ((result (gethash "result" response "")))
      (when (string-match-p (regexp-quote org-sm-comment-registered-signature) result)
        (string-match "<\\(.*\\)>" result)
        (substring (match-string 0 result) 1 -1)))))

(defun org-sm-apiclient-get-element-priority ()
  "Gets priority of current element before any changes made recently (This is a limitation of the api server)."
  (gethash "Priority" (org-sm-apiclient-element-info)))

(defun org-sm-apiclient-item-p ()
  (let ((type (gethash "ElementType" (org-sm-apiclient-element-info))))
    (equal "Item" type)))

(defun org-sm-apiclient-dismissed-p (&optional element-info)
  (let ((status (gethash "ElementStatus" (or element-info (org-sm-apiclient-element-info)))))
    (equal "Dismissed" status)))

(defun org-sm-apiclient-dismiss ()
   (org-sm-apiclient-http-response-p
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current "GET" "dismiss")))

(defun org-sm-apiclient-store-element-id (id)
  (org-sm-apiclient-http-response-p
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current
     "POST"
     "append-comment"
     (list (cons "comment" (concat org-sm-comment-registered-signature "<" (princ id) ">"))))))

(defun org-sm-apiclient-set-element-content (content)
  (org-sm-apiclient-http-response-p
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current
     "POST"
     "set-element-content"
     (list (cons "text" content)))))

(defun org-sm-apiclient-set-element-title (title)
  (org-sm-apiclient-http-response-p
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current
     "POST"
     "set-element-title"
     (list (cons "text" title)))))

(defun org-sm-apiclient-search-element-id (id)
  (org-sm-apiclient-http-response-p
   ; TODO Check to make sure id hasn't been used before?
    (org-sm-apiclient-http-with-sm-server-url-do-and-parse-current
     "POST"
     "goto-first-element-with-comment"
     (list (cons "comment" (princ id))))))

(require 'org-web-tools)
(require 'org-roam-node)

(defvar org-sm-node-current-id nil
  "Contains org-element for current node in learning process.")

(defun org-sm-node-element-type-read-s (type-s)
  "Prompts user to enter element type. Returns element type as string."
  (interactive
   (list
    (completing-read "Choose Element Type: " '(":topic" ":item"))))
  type-s)
  
(defun org-sm-node-postpone-days-read (initial-days)
  "Prompts user to enter days"
  (let ((days (read-number "Enter New Interval (days): " initial-days)))
    (when (and (<= 1 days) (integerp days))
    days)))
  
(defun org-sm-node-priority-read (initial-priority)
  "Prompts user to enter priority"
  (let ((priority (read-number "Enter Priority: " initial-priority)))
    (unless (or (>  priority 100)
                (<= priority 0))
    priority)))

(defun org-sm-capture-node-prepare-finalize-maybe-abort ()
  (when (and org-note-abort
             (org-capture-get :element-type))
    (org-sm-apiclient-dismiss)))

(defun org-sm-capture-node-before-finalize-maybe-back-to-original-element ()
  (when-let ((original-current-id (org-capture-get :sm-extract-original-current-id)))
    (setq org-sm-node-current-id original-current-id)
    (org-sm-apiclient-search-element-id original-current-id)))

(defun org-sm-node-convert-and-export-at-point ()
  "Converts org entry at point to SM node and exports to supermemo as element"
  ; TODO verify that you're at a valid org entry at this point
  (interactive)
  (org-back-to-heading)
  (let ((priority (org-sm-node-priority-read 33.3))
        (type-s (call-interactively 'org-sm-node-element-type-read-s)))
      (org-entry-put (point) "SM_PRIORITY" (number-to-string priority))
      (org-entry-put (point) "SM_ELEMENT_TYPE" type-s)
      (org-sm-node-export-at-point)))

(defun org-sm-node-set-type-tags-at-point (type)
  (save-excursion
    (org-back-to-heading)
    (let ((tags (org-get-tags)))
      (add-to-list 'tags (substring (symbol-name type) 1))
      (add-to-list 'tags "drill")
      (org-set-tags tags))))

(defun org-sm-node-export-at-point (&optional extract-parent-id)
  "Exports node at point to supermemo as element. If EXTRACT-PARENT-ID is non-nil, it creates an extract."
  (let ((content (buffer-substring-no-properties
                  (org-element-property :contents-begin (org-element-at-point))
                  (org-element-property :contents-end (org-element-at-point))))
        ;(title (org-element-property :title (org-element-at-point)))
        (id (or (org-roam-id-at-point) (org-id-get-create)))
        (priority-s (org-entry-get (point) "SM_PRIORITY"))
        (type (intern (org-entry-get (point) "SM_ELEMENT_TYPE"))))
    (message "id: %s" id)
    (setq org-sm-node-current-id id)
    (org-sm-node-set-type-tags-at-point type)
    (when extract-parent-id
      (org-entry-put (point) "SM_EXTRACT_PARENT_ID" extract-parent-id))
    (org-sm-apiclient-element-create type)
    (when priority-s
      (org-sm-apiclient-set-priority (float (string-to-number priority))))
    ;TODO (org-sm-apiclient-set-element-content content)
    ;TODO this causes race conditions that are annoying
    (org-sm-apiclient-store-element-id id)))

(defun org-sm-capture-node-maybe-create ()
  (when-let ((type (org-capture-get :element-type)))
    (let* ((original-link (plist-get org-capture-plist :annotation))
           (original-description
            (replace-regexp-in-string "\\(\\[\\[.*\\]\\[\\)\\(.*?\\)\\]\\]" "\\2" original-link))
           (_ (insert original-description))
           (parent-id (plist-get org-capture-plist :sm-extract-parent-id))
           (priority-s (plist-get org-capture-plist :priority))
           (_ (message "priority is: %s" priority-s))
           (_ (when (plist-get org-capture-plist :ask-priority)
                (setq priority-s (number-to-string (org-sm-node-priority-read (or priority-s "33")))))))
      (message "extract-parent-id: %s" parent-id)
      (org-entry-put (point) "SM_PRIORITY" priority-s)
      (org-entry-put (point) "SM_ELEMENT_TYPE" (symbol-name type))
      (org-sm-node-export-at-point parent-id)
      (org-sm-capture-do-to-original-buffer
       '(progn
          (deactivate-mark))))))

(defun org-sm-capture-node-maybe-smimport ()
    (when-let* ((element-info (org-capture-get :sm-import-element-info))
                (type (cdr (assoc-string
                            (downcase (gethash "ElementType" element-info "topic"))
                            '(("topic" . :topic)
                              ("item" . :item)))))
                (priority (gethash "Priority" element-info ""))
                (contents (org-web-tools--html-to-org-with-pandoc (gethash "Content" element-info "")))
                (title (gethash "Title" element-info "Untitled Element Imported From SM")))
      ; TODO make sure if it's an item and you're ready-to-grade, it doesn't reveal the answer
      (save-excursion
        (message "setting title: %s" title)
        (org-edit-headline title))
      (save-excursion
        (insert "\n" contents))
      (org-sm-node-set-type-tags-at-point type)
      (if-let ((id (org-capture-get :sm-import-id)))
          (org-entry-put (point) "ID" id)
          (setq org-sm-node-current-id id)
          (org-sm-apiclient-store-element-id (org-id-get-create)))
      (org-entry-put (point) "SM_PRIORITY" priority)
      (org-entry-put (point) "SM_ELEMENT_TYPE" (symbol-name type))))

(add-hook 'org-capture-mode-hook #'org-sm-capture-node-maybe-create)
(add-hook 'org-capture-mode-hook #'org-sm-capture-node-maybe-smimport)
(add-hook 'org-capture-prepare-finalize-hook #'org-sm-capture-node-prepare-finalize-maybe-abort)
(add-hook 'org-capture-before-finalize-hook #'org-sm-capture-node-before-finalize-maybe-back-to-original-element)


;; Add the extract templates
(add-to-list 'org-capture-templates
      '("x" "extract" entry (file "~/org/extracts.org")
        "* %? (extract) \n%U\n%a\n\n%i\n" :clock-in t :clock-resume t :element-type :topic))

;; Add the extract templates
(add-to-list 'org-capture-templates
      '("s" "sm-import" entry (file "~/org/extracts.org")
        "* \n%U\n\n%?\n" :clock-in t :clock-resume t))

;TODO Make sure the extracts use org-id in their :annotations
;TODO ^ for this read this: https://www.gnu.org/software/emacs/manual/html_node/org/Handling-Links.html
;TODO To fix this, I might need to do something to org-store-link
;TODO It's really important that these links *just work*
 
(defun org-sm-capture-do-to-original-buffer (fn)
  "Capture the active region of the pdf-view buffer."
  (let* ((orig-buf-name (plist-get org-capture-plist :original-buffer))
         (orig-buf (get-buffer orig-buf-name)))
    (if (buffer-live-p orig-buf)
        (with-current-buffer orig-buf (eval fn))
      (user-error "Buffer %S not alive." orig-buf-name))))

(defun org-sm-node-extract ()
  "TODO Docstring. Remember to make note of the prefix argument for no-immediate-finish"
  (interactive)
  (widen)
  (unwind-protect
    (when (region-active-p)
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (org-sm-apiclient-http-ping)
        (org-ov-highlight-blue)
        (let* ((immediate-finish (not current-prefix-arg))
               (parent-id (let ((org-sm-node-current-id (or (org-roam-id-at-point) (org-id-get-create))))
                            (call-interactively 'org-sm-read-point-set)
                            org-sm-node-current-id))
               ;(_ (message "parent id: %s. original-id: %s" parent-id org-sm-node-current-id))
               (priority (or (org-entry-get (org-id-find parent-id t) "SM_PRIORITY") "33"))
               (_ (message "priority is %s" priority))
               (org-capture-templates
                (if immediate-finish
                    (mapcar (lambda (template)
                              (append template
                                      (list :immediate-finish t)
                                      (list :sm-extract-original-current-id org-sm-node-current-id)
                                      (list :sm-extract-parent-id parent-id)
                                      (list :priority priority)
                                      (when current-prefix-arg (list :ask-priority t))))
                            org-capture-templates)
                  org-capture-templates)))
          (org-capture nil "x"))))
    (deactivate-mark)))

(defun org-sm-node-search-element-id-at-point ()
  (interactive)
  (when-let ((id (org-id-get)))
    (message "id: %s" id)
    (org-sm-apiclient-search-element-id id)))

(defun org-sm-id-goto (id)
  "Same as org-id-goto but returns t upon finding matching id, otherwise it returns nil."
  ;TODO look into ignore-errors
  (interactive "sID: ")
  (when-let ((m (org-id-find id 'marker)))
    (org-pop-to-buffer-same-window (marker-buffer m))
    (goto-char m)
    (move-marker m nil)
    (org-show-context)
    t))

(defun org-sm-node-set-priority ()
  (interactive)
  (let* ((_ (org-sm-goto-current))
        (current-priority (string-to-number (org-entry-get (point) "SM_PRIORITY")))
        (priority (org-sm-node-priority-read current-priority)))
    (org-entry-put (point) "SM_PRIORITY" (number-to-string priority))
    (org-sm-apiclient-set-priority (float priority))))

(defun org-sm-goto-current ()
  (interactive)
  (org-sm-apiclient-current-repetition)
  (call-interactively 'org-sm-node-goto-element-id-or-smimport)
  (setq org-sm-node-current-id (org-roam-id-at-point))) ; TODO should i make this rely on org-roam?

(defun org-sm-node-maybe-dismiss-at-point ()
  (interactive)
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when (and (member "drill" (org-get-tags))
               (equal mystate "DONE")
               (org-sm-node-search-element-id-at-point))
      (org-sm-node-dismiss-at-point))))

(defun org-sm-node-dismiss-at-point ()
  (interactive)
  (when (org-sm-node-search-element-id-at-point)
    (org-sm-apiclient-dismiss)
    (let ((tags (org-get-tags)))
      (add-to-list 'tags "dismissed")
      (org-set-tags tags))))

(defun org-sm-node-postpone ()
  (interactive)
  (when-let ((days (org-sm-node-postpone-days-read)))
    (org-sm-apiclient-postpone days)))

(defun org-sm-goto-next ()
  (interactive)
  (org-sm-apiclient-next-repetition)
  (org-sm-apiclient-current-repetition)
  (call-interactively 'org-sm-node-goto-element-id-or-smimport)
  (evil--jumps-push)
  (setq org-sm-node-current-id (org-roam-id-at-point))) ; TODO should i make this rely on org-roam?

(defun org-sm-read-point-goto ()
  (interactive)
  (when org-sm-node-current-id
    (org-sm-id-goto org-sm-node-current-id)
    (bookmark-jump (concat "sm-" org-sm-node-current-id))))

(defun org-sm-read-point-set ()
  (interactive)
  (when-let* ((id org-sm-node-current-id)
             (bmark-name (concat "sm-" id))
             (pos (point))
             (buffer (if (string-prefix-p "CAPTURE-" (buffer-name))
                         (org-capture-get :buffer)
                       (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (bookmark-set bmark-name)))))

(defun org-sm-node-goto-element-id-or-smimport ()
  "If current element has id, go to node with id. If current element has no Id, import element using org-capture."
  (interactive)
  (widen)
  (let ((id (org-sm-apiclient-get-element-id)))
    (message "original id is %s" id)
    (when-let ((_ (or (not id) (not (org-sm-id-goto id))))
               (element-info (org-sm-apiclient-element-info))
               (_ (not (org-sm-apiclient-dismissed-p element-info)))
               (org-capture-templates
                (mapcar (lambda (template)
                          (append template
                                  (list :sm-import-element-info element-info)
                                  (when id (list :sm-import-id id))))
                          org-capture-templates)))
      (org-capture nil "s"))))

(defun org-sm-maybe-capture-buffer-finalize ()
  "If buffer at point is a capture buffer, finalize it."
  (when (string-prefix-p "CAPTURE" (buffer-name (current-buffer))) (org-capture-finalize)))
  
(add-hook 'org-after-todo-state-change-hook 'org-sm-node-maybe-dismiss-at-point 'append)
(advice-add 'org-sm-goto-next :before #'org-sm-maybe-capture-buffer-finalize)
(advice-add 'org-sm-node-goto-element-id-or-smimport :after #'org-narrow-to-subtree)
(advice-add 'org-sm-node-goto-element-id-or-smimport :after #'outline-show-all)
(advice-add 'org-sm-node-extract :after #'outline-show-all)
;NEXT Make an advice that clocks in next supermemo elements
;NEXT Make an advice that clocks in current supermemo elements
;NEXT Make an advice that stops org-sm from clocking if it's during work hours
;TODO Display also the supermemo references
;TODO Make the highlights local file info stuff hide in the PROPERTIES

;TODO If the supermemo element has an id but there's no corresponding org-id, ask user if they want to delete the supermemo element

;TODO (defun org-sm-node-import-current-element ()
;  "Import current element information from SM."
;TODO I need to make it obvious when an element has been imported or not, I need a hot key for "go to the current element in supermemo" that opens the import dialog if not found

;TODO Make an export function:
;TODO Make a function that takes an org entry/element at point and then adds it to supermemo and then adds the :drill: tag and stuff
;TODO It will open a completion dialog with the element types (currently just topic and item)
; This will be useful for when I'm in org capture or whatever, and I just made a note or something, and I want it tracked in supermemo

;TODO (defun org-sm-node-import-current-element ()
;  "Import current element information from SM."
; We need a function that will get run when org-sm doesn't detect that the element has an ID marked into it, or it doesn't detect an org-id matching it, it will ask the user if they would like to import it into emacs, this will open up a capture template, and that's what I'll work on next
;TODO Make sure when you go-to element, you check that the element is dismissed or not

;(global-set-key (kbd "C-c x") 'org-sm-node-extract)
;(global-set-key (kbd "C-c s") 'org-sm-goto-current)
;(global-set-key (kbd "C-c S") 'org-sm-goto-next)
;;(global-set-key (kbd "C-c S") 'org-sm-node-search-element-id-at-point)
;(global-set-key (kbd "C-c X") 'org-sm-node-convert-and-export-at-point) ; TODO this is just fo rtesting, change key sym later

(spacemacs/set-leader-keys
  "ax" 'org-sm-node-extract
  "asc" 'org-sm-goto-current
  "asg" 'org-sm-read-point-goto
  "asm" 'org-sm-read-point-set
  "asp" 'org-sm-node-set-priority
  "asr" 'org-sm-node-postpone
  "asn" 'org-sm-goto-next
  "sn" 'org-sm-goto-next
  "ase" 'org-sm-goto-next)

(define-key evil-visual-state-map (kbd "C-x C-x") 'org-sm-node-extract)

(defun org-sm-new-sm-element-with-id (id)
  "Communicates to the SM api server to create a new element with title set to id."
  ())


