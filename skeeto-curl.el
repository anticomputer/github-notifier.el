;;; skeeto-curl.el --- curl backend from elfeed -*- lexical-binding: t; -*-

;;; Comments:

;; An alternative to `url-retrieve' and `url-queue' that fetches URLs
;; using the curl command line program.

;; The API is three functions:

;; * `skeeto-curl-retrieve'
;; * `skeeto-curl-retrieve-synchronously'
;; * `skeeto-curl-enqueue'

;; And has four buffer-local variables for use in callbacks:

;; * `skeeto-curl-headers'
;; * `skeeto-curl-status-code'
;; * `skeeto-curl-error-message'
;; * `skeeto-curl-location'

;; The buffer delivered to callbacks may contain multiple requests. It
;; will be narrowed to the specific content for the current request.
;; It's vitally important that callbacks do not kill the buffer
;; because it may be needed for other callbacks. It also means the
;; buffer won't necessarily be around when the callback returns.
;; Callbacks should also avoid editing the buffer, though this
;; generally shouldn't impact other requests.

;; Sometimes Elfeed asks curl to retrieve multiple requests and
;; deliver them concatenated. Due to the possibility of HTTP/1.0 being
;; involved — and other ambiguous-length protocols — there's no
;; perfectly unambiguous way to split the output. To work around this,
;; I use curl's --write-out to insert a randomly-generated token after
;; each request. It's highly unlikely (1 in ~1e38) that this token
;; will appear in content, so I can use it to identify the end of each
;; request.

;; this code was lifted and modified from: https://github.com/skeeto/elfeed

;;; Code:

(require 'url)
(require 'cl-lib)

(defcustom skeeto-curl-program-name "curl"
  "Name/path by which to invoke the curl program."
  :group 'skeeto-curl
  :type 'string)

(defcustom skeeto-curl-max-connections 16
  "Maximum number of concurrent fetches."
  :group 'skeeto-curl
  :type 'integer)

(defcustom skeeto-curl-timeout 30
  "Maximum number of seconds a fetch is allowed to take once started."
  :group 'skeeto-curl
  :type 'integer)

(defcustom skeeto-curl-extra-arguments ()
  "A list of additional arguments to pass to cURL.
These extra arguments are appended after Elfeed's own arguments,
and care must be taken to not interfere with Elfeed's needs. The
guideline is to avoid arguments that change anything about cURL's
output format."
  :group 'skeeto-curl
  :type '(repeat string))

(defvar skeeto-curl-queue ()
  "List of pending curl requests.")

(defvar skeeto-curl-queue-active 0
  "Number of concurrent requests currently active.")

(defvar-local skeeto-curl-headers nil
  "Alist of HTTP response headers.")

(defvar-local skeeto-curl-status-code nil
  "Numeric HTTP response code, nil for non-HTTP protocols.")

(defvar-local skeeto-curl-error-message nil
  "Human-friendly message describing the error.")

(defvar-local skeeto-curl-location nil
  "Actual URL fetched (after any redirects).")

(defvar-local skeeto-curl--regions ()
  "List of markers bounding separate requests.")

(defvar-local skeeto-curl--requests ()
  "List of URL / callback pairs for the current buffer.")

(defvar-local skeeto-curl--token nil
  "Unique token that splits requests.")

(defvar-local skeeto-curl--refcount nil
  "Number of callbacks waiting on the current buffer.")

(defvar skeeto-curl--error-codes
  '((1 . "Unsupported protocol.")
    (2 . "Failed to initialize.")
    (3 . "URL malformed. The syntax was not correct.")
    (4 . "A feature or option that was needed to perform the desired request was not enabled or was explicitly disabled at build-time.")
    (5 . "Couldn't resolve proxy. The given proxy host could not be resolved.")
    (6 . "Couldn't resolve host. The given remote host was not resolved.")
    (7 . "Failed to connect to host.")
    (8 . "FTP weird server reply. The server sent data curl couldn't parse.")
    (9 . "FTP access denied.")
    (11 . "FTP weird PASS reply.")
    (13 . "FTP weird PASV reply.")
    (14 . "FTP weird 227 format.")
    (15 . "FTP can't get host.")
    (16 . "A problem was detected in the HTTP2 framing layer.")
    (17 . "FTP couldn't set binary.")
    (18 . "Partial file. Only a part of the file was transferred.")
    (19 . "FTP couldn't download/access the given file, the RETR (or similar) command failed.")
    (21 . "FTP quote error. A quote command returned error from the server.")
    (22 . "HTTP page not retrieved.")
    (23 . "Write error.")
    (25 . "FTP couldn't STOR file.")
    (26 . "Read error. Various reading problems.")
    (27 . "Out of memory. A memory allocation request failed.")
    (28 . "Operation timeout.")
    (30 . "FTP PORT failed.")
    (31 . "FTP couldn't use REST.")
    (33 . "HTTP range error. The range \"command\" didn't work.")
    (34 . "HTTP post error. Internal post-request generation error.")
    (35 . "SSL connect error. The SSL handshaking failed.")
    (36 . "FTP bad download resume.")
    (37 . "FILE couldn't read file.")
    (38 . "LDAP bind operation failed.")
    (39 . "LDAP search failed.")
    (41 . "Function not found. A required LDAP function was not found.")
    (42 . "Aborted by callback.")
    (43 . "Internal error. A function was called with a bad parameter.")
    (45 . "Interface error. A specified outgoing interface could not be used.")
    (47 . "Too many redirects.")
    (48 . "Unknown option specified to libcurl.")
    (49 . "Malformed telnet option.")
    (51 . "The peer's SSL certificate or SSH MD5 fingerprint was not OK.")
    (52 . "The server didn't reply anything, which here is considered an error.")
    (53 . "SSL crypto engine not found.")
    (54 . "Cannot set SSL crypto engine as default.")
    (55 . "Failed sending network data.")
    (56 . "Failure in receiving network data.")
    (58 . "Problem with the local certificate.")
    (59 . "Couldn't use specified SSL cipher.")
    (60 . "Peer certificate cannot be authenticated with known CA certificates.")
    (61 . "Unrecognized transfer encoding.")
    (62 . "Invalid LDAP URL.")
    (63 . "Maximum file size exceeded.")
    (64 . "Requested FTP SSL level failed.")
    (65 . "Sending the data requires a rewind that failed.")
    (66 . "Failed to initialise SSL Engine.")
    (67 . "The user name, password, or similar was not accepted and curl failed to log in.")
    (68 . "File not found on TFTP server.")
    (69 . "Permission problem on TFTP server.")
    (70 . "Out of disk space on TFTP server.")
    (71 . "Illegal TFTP operation.")
    (72 . "Unknown TFTP transfer ID.")
    (73 . "File already exists (TFTP).")
    (74 . "No such user (TFTP).")
    (75 . "Character conversion failed.")
    (76 . "Character conversion functions required.")
    (77 . "Problem with reading the SSL CA cert (path? access rights?).")
    (78 . "The resource referenced in the URL does not exist.")
    (79 . "An unspecified error occurred during the SSH session.")
    (80 . "Failed to shut down the SSL connection.")
    (82 . "Could not load CRL file, missing or wrong format (added in 7.19.0).")
    (83 . "Issuer check failed (added in 7.19.0).")
    (84 . "The FTP PRET command failed")
    (85 . "RTSP: mismatch of CSeq numbers")
    (86 . "RTSP: mismatch of Session Identifiers")
    (87 . "unable to parse FTP file list")
    (88 . "FTP chunk callback reported error")
    (89 . "No connection available, the session will be queued")
    (90 . "SSL public key does not matched pinned public key")))

(defvar skeeto-curl--capabilities-cache
  (make-hash-table :test 'eq :weakness 'key)
  "Used to avoid invoking curl more than once for version info.")

(defun skeeto-curl-get-capabilities ()
  "Return capabilities plist for the curl at `skeeto-curl-program-name'.
:version     -- cURL's version string
:compression -- non-nil if --compressed is supported"
  (let* ((cache skeeto-curl--capabilities-cache)
         (cache-value (gethash skeeto-curl-program-name cache)))
    (if cache-value
        cache-value
      (with-temp-buffer
        (call-process skeeto-curl-program-name nil t nil "--version")
        (let ((version
               (progn
                 (setf (point) (point-min))
                 (when (re-search-forward "[.0-9]+" nil t)
                   (match-string 0))))
              (compression
               (progn
                 (setf (point) (point-min))
                 (not (null (re-search-forward "libz\\>" nil t))))))
          (setf (gethash skeeto-curl-program-name cache)
                `(:version ,version :compression ,compression)))))))

(defun skeeto-curl-get-version ()
  "Return the version of curl for `skeeto-curl-program-name'."
  (plist-get (skeeto-curl-get-capabilities) :version))
(make-obsolete 'skeeto-curl-get-version 'skeeto-curl-get-capabilities "3.0.1")

(defun skeeto-curl--token ()
  "Return a unique, random string that prints as a symbol without escapes.
This token is used to split requests. The % is excluded since
it's special to --write-out."
  (let* ((token (make-string 22 ?=))
         (set "!$&*+-/0123456789:<>@ABCDEFGHIJKLMNOPQRSTUVWXYZ^_\
abcdefghijklmnopqrstuvwxyz|~"))
    (prog1 token ; workaround bug#16206
      (dotimes (i (- (length token) 2))
        (setf (aref token (1+ i)) (aref set (cl-random (length set))))))))

(defun skeeto-curl--parse-write-out ()
  "Parse curl's write-out (-w) messages into `skeeto-curl--regions'."
  (widen)
  (setf (point) (point-max)
        skeeto-curl--regions ())
  (while (> (point) (point-min))
    (search-backward skeeto-curl--token)
    (cl-decf (point))
    (let ((end (point)))
      (cl-destructuring-bind (_ . header) (read (current-buffer))
        (setf (point) end)
        ;; Find next sentinel token
        (if (search-backward skeeto-curl--token nil t)
            (search-forward ")" nil t)
          (setf (point) (point-min)))
        (let* ((header-start (point))
               (header-end (+ (point) header))
               (content-start (+ (point) header))
               (content-end end)
               (regions (list header-start header-end
                              content-start content-end))
               (markers (cl-loop for p in regions
                                 for marker = (make-marker)
                                 collect (set-marker marker p))))
          (push markers skeeto-curl--regions))))))

(defun skeeto-curl--narrow (kind n)
  "Narrow to Nth region of KIND (:header, :content)."
  (let ((region (nth n skeeto-curl--regions)))
    (cl-destructuring-bind (h-start h-end c-start c-end) region
      (cl-ecase kind
        (:header (narrow-to-region h-start h-end))
        (:content (narrow-to-region c-start c-end))))))

(defun skeeto-curl--parse-headers ()
  "Parse the current HTTP response headers into buffer-locals.
Sets `skeeto-curl-headers'and `skeeto-curl-status-code'.
Use `skeeto-curl--narrow' to select a header."
  (when (> (- (point-max) (point-min)) 0)
    (setf (point) (point-max))
    (re-search-backward "HTTP/[.0-9]+ +\\([0-9]+\\)")
    (setf skeeto-curl-status-code (string-to-number (match-string 1)))
    (cl-loop initially (setf (point) (point-max))
             while (re-search-backward "^\\([^:]+\\): +\\([^\r\n]+\\)" nil t)
             for key = (downcase (match-string 1))
             for value = (match-string 2)
             collect (cons key value) into headers
             finally (setf skeeto-curl-headers headers))))

(defun skeeto-curl--decode ()
  "Try to decode the buffer based on the headers."
  (let ((content-type (cdr (assoc "Content-Type" skeeto-curl-headers))))
    (if (and content-type (string-match "charset=\\(.+\\)" content-type))
        (decode-coding-region (point-min) (point-max)
                              (coding-system-from-name
                               (match-string 1 content-type)))
      (decode-coding-region (point-min) (point-max) 'utf-8))))

(defun skeeto-remove-dot-segments (input)
  "Relative URL algorithm as described in RFC 3986 §5.2.4."
  (cl-loop
   with output = ""
   for s = input
   then (cond
         ((string-match-p "^\\.\\./" s)
          (substring s 3))
         ((string-match-p "^\\./" s)
          (substring s 2))
         ((string-match-p "^/\\./" s)
          (substring s 2))
         ((string-match-p "^/\\.$" s) "/")
         ((string-match-p "^/\\.\\./" s)
          (setf output (replace-regexp-in-string "/?[^/]*$" "" output))
          (substring s 3))
         ((string-match-p "^/\\.\\.$" s)
          (setf output (replace-regexp-in-string "/?[^/]*$" "" output))
          "/")
         ((string-match-p "^\\.\\.?$" s)
          "")
         ((string-match "^/?[^/]*" s)
          (setf output (concat output (match-string 0 s)))
          (replace-regexp-in-string "^/?[^/]*" "" s)))
   until (zerop (length s))
   finally return output))

(defun skeeto-update-location (old-url new-url)
  "Return full URL for maybe-relative NEW-URL based on full OLD-URL."
  (if (null new-url)
      old-url
    (let ((old (url-generic-parse-url old-url))
          (new (url-generic-parse-url new-url)))
      (cond
       ;; Is new URL absolute already?
       ((url-type new) new-url)
       ;; Empty is a special case (clear fragment)
       ((equal new-url "")
        (setf (url-target old) nil)
        (url-recreate-url old))
       ;; Does it start with //? Append the old protocol.
       ((url-fullness new) (concat (url-type old) ":" new-url))
       ;; Is it a relative path?
       ((not (string-match-p "^/" new-url))
        (let* ((old-dir (or (file-name-directory (url-filename old)) "/"))
               (concat (concat old-dir new-url))
               (new-file (skeeto-remove-dot-segments concat)))
          (setf (url-filename old) nil
                (url-target old) nil
                (url-attributes old) nil
                (url-filename old) new-file)
          (url-recreate-url old)))
       ;; Replace the relative part.
       ((progn
          (setf (url-filename old) (skeeto-remove-dot-segments new-url)
                (url-target old) nil
                (url-attributes old) nil)
          (url-recreate-url old)))))))


(defun skeeto-curl--final-location (location headers)
  "Given start LOCATION and HEADERS, find the final location."
  (cl-loop for (key . value) in headers
           when (equal key "location")
           do (setf location (skeeto-update-location location value))
           finally return location))

(defun skeeto-curl--args (url token &optional headers method data)
  "Build an argument list for curl for URL.
URL can be a string or a list of URL strings."
  (let* ((args ())
         (capabilities (skeeto-curl-get-capabilities)))
    (push "--disable" args)
    (when (plist-get capabilities :compression)
      (push "--compressed" args))
    (push "--silent" args)
    (push "--location" args)
    (push (format "-w(%s . %%{size_header})" token) args)
    (push (format "-m%s" skeeto-curl-timeout) args)
    (push "-D-" args)
    (dolist (header headers)
      (cl-destructuring-bind (key . value) header
        (push (format "-H%s: %s" key value) args)))
    (when method (push (format "-X%s" method) args))
    (when data (push (format "-d%s" data) args))
    (setf args (nconc (reverse skeeto-curl-extra-arguments) args))
    (if (listp url)
        (nconc (nreverse args) url)
      (nreverse (cons url args)))))

(defun skeeto-curl--prepare-response (url n)
  "Prepare response N for delivery to user."
  (skeeto-curl--narrow :header n)
  (skeeto-curl--parse-headers)
  (setf skeeto-curl-location
        (skeeto-curl--final-location url skeeto-curl-headers))
  (skeeto-curl--narrow :content n)
  (skeeto-curl--decode)
  (current-buffer))

(cl-defun skeeto-curl-retrieve-synchronously (url &key headers method data)
  "Retrieve the contents for URL and return a new buffer with them.

HEADERS is an alist of additional headers to add to the HTTP request.
METHOD is the HTTP method to use.
DATA is the content to include in the request."
  (with-current-buffer (generate-new-buffer " *curl*")
    (setf skeeto-curl--token (skeeto-curl--token))
    (let ((args (skeeto-curl--args url skeeto-curl--token headers method data))
          (coding-system-for-read 'binary))
      (apply #'call-process skeeto-curl-program-name nil t nil args))
    (skeeto-curl--parse-write-out)
    (skeeto-curl--prepare-response url 0)))

(defun skeeto-curl--call-callback (buffer n url cb)
  "Prepare the buffer for callback N and call it."
  (let ((result nil))
    (with-current-buffer buffer
      (setf skeeto-curl-error-message "unable to parse curl response")
      (unwind-protect
          (progn
            (skeeto-curl--prepare-response url n)
            (if (and (>= skeeto-curl-status-code 400)
                     (<= skeeto-curl-status-code 599))
                (setf skeeto-curl-error-message
                      (format "HTTP %d" skeeto-curl-status-code))
              (setf result t
                    skeeto-curl-error-message nil)))
        ;; Always call callback
        (unwind-protect
            (funcall cb result)
          ;; Always clean up
          (when (zerop (cl-decf skeeto-curl--refcount))
            (kill-buffer)))))))

(defun skeeto-curl--fail-callback (buffer cb)
  "Inform the callback the request failed."
  (with-current-buffer buffer
    (unwind-protect
        (funcall cb nil)
      (when (zerop (cl-decf skeeto-curl--refcount))
        (kill-buffer)))))

(defun skeeto-curl--sentinel (process status)
  "Manage the end of a curl process' life."
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      ;; Fire off callbacks in separate interpreter turns so they can
      ;; each fail in isolation from each other.
      (if (equal status "finished\n")
          (cl-loop with handler = #'skeeto-curl--call-callback
                   initially do (skeeto-curl--parse-write-out)
                   for (url . cb) in skeeto-curl--requests
                   for n upfrom 0
                   do (run-at-time 0 nil handler buffer n url cb))
        (if (string-match "exited abnormally with code \\([0-9]+\\)" status)
            (let* ((code (string-to-number (match-string 1 status)))
                   (message (cdr (assoc code skeeto-curl--error-codes))))
              (setf skeeto-curl-error-message
                    (format "(%d) %s" code
                            (or message "Unknown curl error!"))))
          (setf skeeto-curl-error-message status))
        (cl-loop with handler = #'skeeto-curl--fail-callback
                 for (_ . cb) in skeeto-curl--requests
                 do (run-at-time 0 nil handler buffer cb))))))

(cl-defun skeeto-curl-retrieve (url cb &key headers method data)
  "Retrieve URL contents asynchronously, calling CB with one status argument.

The callback must *not* kill the buffer!

The destination buffer is set at the current buffer for the
callback.

HEADERS is an alist of additional headers to add to HTTP requests.
METHOD is the HTTP method to use.
DATA is the content to include in the request.

URL can be a list of URLs, which will fetch them all in the same
curl process. In this case, CB can also be either a list of the
same length, or just a single function to be called once for each
URL in the list. Headers will be common to all requests. A TCP or
DNS failure in one will cause all to fail, but 4xx and 5xx
results will not."
  (with-current-buffer (generate-new-buffer " *curl*")
    (setf skeeto-curl--token (skeeto-curl--token))
    (let* ((coding-system-for-read 'binary)
           (process-connection-type nil)
           (args (skeeto-curl--args url skeeto-curl--token headers method data))
           (process (apply #'start-process "skeeto-curl" (current-buffer)
                           skeeto-curl-program-name args)))
      (prog1 process
        (if (listp url)
            (progn
              (when (functionp cb)
                (setf cb (make-list (length url) cb)))
              (setf skeeto-curl--requests (cl-mapcar #'cons url cb)
                    skeeto-curl--refcount (length url)))
          (push (cons url cb) skeeto-curl--requests)
          (setf skeeto-curl--refcount 1))
        (setf (process-sentinel process) #'skeeto-curl--sentinel)))))

(defun skeeto-curl--request-key (url headers method data)
  "Try to fetch URLs with matching keys at the same time."
  (unless (listp url)
    (let* ((urlobj (url-generic-parse-url url)))
      (list (url-type urlobj)
            (url-host urlobj)
            (url-portspec urlobj)
            headers
            method
            data))))

(defun skeeto-curl--queue-consolidate (queue-in)
  "Group compatible requests together and return a new queue.
Compatible means the requests have the same protocol, domain,
port, headers, method, and body, allowing them to be used safely
in the same curl invocation."
  (let ((table (make-hash-table :test 'equal))
        (keys ())
        (queue-out ()))
    (dolist (entry queue-in)
      (cl-destructuring-bind (url _ headers method data) entry
        (let* ((key (skeeto-curl--request-key url headers method data)))
          (push key keys)
          (push entry (gethash key table nil)))))
    (dolist (key (nreverse keys))
      (let ((entry (gethash key table)))
        (when entry
          (let ((rotated (list (nreverse (cl-mapcar #'car entry))
                               (nreverse (cl-mapcar #'cadr entry))
                               (cl-caddar entry)
                               (elt (car entry) 3)
                               (elt (car entry) 4))))
            (push rotated queue-out)
            (setf (gethash key table) nil)))))
    (nreverse queue-out)))

(defun skeeto-curl--queue-wrap (cb)
  "Wrap the curl CB so that it operates the queue."
  (lambda (status)
    (cl-decf skeeto-curl-queue-active)
    (skeeto-curl--run-queue)
    (funcall cb status)))

(defvar skeeto-curl--run-queue-queued nil
  "Non-nil if run-queue has already been queued for the next turn.")

(defun skeeto-curl--run-queue ()
  "Possibly fire off some new requests."
  (when skeeto-curl--run-queue-queued
    (setf skeeto-curl--run-queue-queued nil
          ;; Try to consolidate the new requests.
          skeeto-curl-queue
          (skeeto-curl--queue-consolidate skeeto-curl-queue)))
  (while (and (< skeeto-curl-queue-active skeeto-curl-max-connections)
              (> (length skeeto-curl-queue) 0))
    (cl-destructuring-bind (url cb headers method data) (pop skeeto-curl-queue)
      (message "retrieve %s" url)
      (cl-incf skeeto-curl-queue-active 1)
      (skeeto-curl-retrieve
       url
       (if (functionp cb)
           (skeeto-curl--queue-wrap cb)
         (cons (skeeto-curl--queue-wrap (car cb))
               (cdr cb)))
       :headers headers
       :method method
       :data data))))

(cl-defun skeeto-curl-enqueue (url cb &key headers method data)
  "Just like `skeeto-curl-retrieve', but restricts concurrent fetches."
  (unless (or (stringp url)
              (and (listp url) (cl-every #'stringp url)))
    ;; Signal error synchronously instead of asynchronously in the timer
    (signal 'wrong-type-argument (list 'string-p-or-string-list-p url)))
  (let ((entry (list url cb headers method data)))
    (setf skeeto-curl-queue (nconc skeeto-curl-queue (list entry)))
    (unless skeeto-curl--run-queue-queued
      (run-at-time 0 nil #'skeeto-curl--run-queue)
      (setf skeeto-curl--run-queue-queued t))))

(provide 'skeeto-curl)

;;; skeeto-curl.el ends here
