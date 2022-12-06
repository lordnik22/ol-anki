;;; ol-anki.el - Support for links to anki decks in Org mode

;; Copyright (C) 2022 lordnik22

;; Author: lordnik22
;; Version: 1.0.0
;; Keywords: extensions
;; URL: https://github.com/lordnik22
;; Package-Requires: ((emacs "28") (request "0.8.0"))

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;; Commentary:
;;; Add a new org-link-type: anki. When opened, will start anki and focus the deck specified by the link.
;;; Code:
(require 'request)
(require 'ol)

(org-link-set-parameters "anki"
			 :follow #'ol-anki-open
			 :export #'ol-anki-export
			 :store #'ol-anki-store-link
			 :complete #'ol-anki-complete-anki)

(defcustom ol-anki-command 'anki
  "The Emacs command to be used to display a anki deck."
  :group 'org-link
  :type '(symbol))

(defun ol-anki-open (name _)
  "Visit the anki deck named ‘NAME’.
‘NAME’ should be a deck that can exists in you anki-program."
  (shell-command "anki && xdotool windowactivate `xdotool search --pid \\`pgrep anki\\``")
  (ol-anki-api-call-result "guiDeckOverview" (cons 'name name)))

(defun ol-anki-store-link ()
  "Not implemented because anki is an external program"
  (error "Can’t store anki link because anki is an external program"))

(defun ol-anki-export (link description format _)
  "Export a anki deck link from Org files."
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" link desc))
      (`latex (format "\\href{%s}{%s}" link desc))
      (`texinfo (format "@uref{%s,%s}" link desc))
      (`ascii (format "%s (%s)" desc link))
      (_ link)))

(defun ol-anki-complete-anki ()
  "Completion with all existing anki decks when creating anki deck links."
  (concat (symbol-name ol-anki-command) ":"
	  (ido-completing-read "Choose Anki deck: " (ol-anki-deck-names))))

;;; Copied from https://github.com/orgtre/anki-editor/blob/master/anki-editor.el and prefixed with ol-anki
(defcustom ol-anki-api-host "127.0.0.1"
  "The network address AnkiConnect is listening on."
  :type 'string)

(defcustom ol-anki-api-port "8765"
  "The port number AnkiConnect is listening on."
  :type 'string)

(defconst ol-anki-api-version 6)

(defun ol-anki-deck-names ()
  "Get all decks names from Anki."
  (ol-anki-api-call-result "deckNames"))

(cl-defun ol-anki--fetch (url
			      &rest settings
			      &key
			      (type "GET")
			      data success _error
			      (parser 'buffer-string)
			      &allow-other-keys)
  "Fetch URL using curl.
The api is borrowed from request.el."
;; This exists because request.el's sync mode calls curl asynchronously under
;; the hood, which doesn't work on some machines (like mine) where the process
;; sentinel never gets called. After some debugging of Emacs, it seems that in
;; 'process.c' the pselect syscall to the file descriptor of inotify used by
;; 'autorevert' always returns a nonzero value and causes 'status_notify' never
;; being called. To determine whether it's a bug in Emacs and make a patch
;; requires more digging.
  (let ((tempfile (make-temp-file "emacs-ol-anki"))
	(responsebuf (generate-new-buffer " *ol-anki-curl*")))
    (when data
      (with-temp-file tempfile
	(setq buffer-file-coding-system 'utf-8)
	(set-buffer-multibyte t)
	(insert data)))
    (unwind-protect
	(with-current-buffer responsebuf
	  (apply #'call-process "curl" nil t nil (list
						  url
						  "--silent"
						  "-X" type
						  "--data-binary"
						  (concat "@" tempfile)))

	  (goto-char (point-min))
	  (when success
	    (apply success (list :data (funcall parser)))))
      (kill-buffer responsebuf)
      (delete-file tempfile))))

(defun ol-anki-api-call (action &rest params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((payload (list :action action :version ol-anki-api-version))
	(_request-backend 'curl)
	(json-array-type 'list)
	reply err)

    (when params
      (plist-put payload :params params))

    (ol-anki--fetch (format "http://%s:%s"
				ol-anki-api-host
				ol-anki-api-port)
			:type "POST"
			:parser 'json-read
			:data (json-encode payload)
			:success (cl-function
				  (lambda (&key data &allow-other-keys)
				    (setq reply data)))
			:error (cl-function
				(lambda (&key error-thrown &allow-other-keys)
				  (setq err (string-trim (cdr error-thrown)))))
			:sync t)
    (when err
      (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))

(defun ol-anki-api-call-result (&rest args)
  "Invoke AnkiConnect with ARGS and return the result from response.
Raise an error if applicable."
  (let-alist (apply #'ol-anki-api-call args)
    (when .error (error .error))
   .result
    ))

(provide 'ol-anki)
;;; ol-anki.el ends here
