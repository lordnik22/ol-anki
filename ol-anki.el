;;; ol-anki.el - Support for links to anki decks in Org mode
(require 'request)
(require 'ol)

(defcustom anki-editor-anki-connect-listening-address
  "127.0.0.1"
  "The network address AnkiConnect is listening.")

(defcustom anki-editor-anki-connect-listening-port
  "8765"
  "The port number AnkiConnect is listening.")

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
  (anki-editor--anki-connect-invoke-result "guiDeckOverview" (list (cons 'name name))))

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

(defun anki-editor--anki-connect-action (action &optional params version)
  (let (a)
    (when version
      (push `(version . ,version) a))
    (when params
      (push `(params . ,params) a))
    (push `(action . ,action) a)))

(defun anki-editor--anki-connect-invoke (action &optional params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((request-body (json-encode (anki-editor--anki-connect-action action params 5)))
	(request-backend 'curl)
	(json-array-type 'list)
	reply err)

    (let ((response (request (format "http://%s:%s"
				     anki-editor-anki-connect-listening-address
				     anki-editor-anki-connect-listening-port)
			     :type "POST"
			     :parser 'json-read
			     :data request-body
			     :success (cl-function (lambda (&key data &allow-other-keys)
						     (setq reply data)))
			     :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
						   (setq err (string-trim (cdr error-thrown)))))
			     :sync t)))

      ;; HACK: With sync set to t, `request' waits for curl process to
      ;; exit, then response data becomes available, but callbacks
      ;; might not be called right away but at a later time, that's
      ;; why here we manually invoke callbacks to receive the result.
      (unless (request-response-done-p response)
	(request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))

    (when err (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))

(defun ol-anki-complete-anki ()
  "Completion with all existing anki decks when creating anki deck links."
  (concat (symbol-name ol-anki-command) ":"
	  (ido-completing-read "Choose Anki deck: " (anki-editor-deck-names))))

(defmacro anki-editor--anki-connect-invoke-result (&rest args)
  "Invoke AnkiConnect with ARGS, return the result from response or raise an error."
  `(let-alist (anki-editor--anki-connect-invoke ,@args)
     (when .error (error .error))
     .result))

(defun anki-editor-deck-names ()
  "Get all decks names from Anki."
  (anki-editor--anki-connect-invoke-result "deckNames"))

(provide 'ol-anki)
;;; ol-anki.el ends here
