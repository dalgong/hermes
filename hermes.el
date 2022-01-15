;;; hermes.el --- A Mercurial frontend               -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <jaeyoon@localhost>
;; Keywords: vc

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(require 'ewoc)

(require 'transient)
(require 'with-editor)

(add-to-list 'auto-mode-alist
             (cons (rx (or string-start "/")
                       "hg-editor-" (+ (any alphanumeric "-_")) ".commit.hg.txt"
                       string-end)
                   #'with-editor-mode))

(defvar hermes--log-revset "reverse(.~3::)")
(defvar hermes--hg-commands '("hg" "--color=never" "--pager=never"))
(defvar hermes--log-template
  (concat "changeset: {node|short}\\n"
          "summary: {desc|firstline}\\n"
          "date: {date|age}\\n"
          "{parents % \"parent: {node|short}\\n\"}"
          "{tags % \"tag: {tag}\\n\"}"))

(defclass hermes--base ()
  ((parent      :initarg :parent      :initform nil)
   (expanded    :initarg :expanded    :initform nil)
   (node        :initarg :node        :initform nil)))
(defclass hermes--changeset (hermes--base)
  ((title       :initarg :title       :initform nil)
   (rev         :initarg :rev         :initform nil)
   (current     :initarg :current     :initform nil)
   (tags        :initarg :tags)
   (summary     :initarg :summary)
   (props       :initarg :props)
   (files       :initarg :files       :initform nil)
   (parent-revs :initarg :parent-revs :initform nil)
   (child-revs  :initarg :child-revs  :initform nil)))
(defclass hermes--file (hermes--base)
  ((file        :initarg :file)
   (rev         :initarg :rev)
   (status      :initarg :status)
   (marked      :initarg :marked      :initform nil)
   (hunks       :initarg :hunks       :initform nil)))
(defclass hermes--hunk (hermes--base)
  ((lines       :initarg :lines)))

(defclass hermes--shelve (hermes--base)
  ((name        :initarg :name)
   (when        :initarg :when)
   (message     :initarg :message)
   (files       :initarg :files       :initform nil)))

;; generic methods
(cl-defgeneric hermes--print (data))
(defun hermes--format-changeset-line (data &optional faces)
  (concat (propertize (oref data rev)
                      'face (cons 'font-lock-type-face faces))
          " "
          (mapconcat (lambda (tag)
                       (propertize tag
                                   'face (cons 'font-lock-keyword-face faces)))
                     (oref data tags)
                     " ")))
(cl-defmethod hermes--print ((data hermes--changeset))
  (let ((faces (and (oref data current) (list 'bold))))
    (if (oref data title)
        (insert (hermes--indent data)
                (propertize (oref data title) 'face 'bold))
      (insert (hermes--indent data t)
              (hermes--format-changeset-line data faces))
      (let* ((date (cadr (assq 'date (oref data props))))
             (padding (and date
                           (- (window-width)
                              (current-column)
                              (length date)
                              1))))
        (when (and date (> padding 0))
          (insert (make-string padding ?\s)
                  (propertize date 'face 'font-lock-doc-face))))
      (insert "\n"
              (hermes--indent data)
              (propertize (oref data summary) 'face (cons 'italic faces))))))
(cl-defmethod hermes--print ((data hermes--file))
  (insert (hermes--indent data)
          "  " (propertize (oref data status) 'face 'font-lock-keyword-face) " "
          (if (oref data marked)
              (propertize (oref data file) 'face 'underline)
              (oref data file))))
(cl-defmethod hermes--print ((data hermes--hunk))
  (let* ((hunk-header (car (oref data lines)))
         (line-num (and (string-match
                         "@@ -[0-9]+,[0-9]+ [+]\\([0-9]+\\),"
                         hunk-header)
                        (string-to-number (match-string 1 hunk-header)))))
    (dolist (line (oref data lines))
      (let ((face (cond ((string-match "^=" line)
                         'bold)
                        ((string-match "^-" line)
                         'diff-removed)
                        ((string-match "^[+]" line)
                         'diff-added)
                        (t
                         'diff-context))))
        (insert (propertize line 'face face))
        (when line-num
          (put-text-property (point-at-bol) (point-at-eol)
                             'hermes-line-num line-num)
          (unless (member (char-after (point-at-bol)) '(?- ?@))
            (cl-incf line-num)))
        (insert "\n")))))
(cl-defmethod hermes--print ((data hermes--shelve))
  (insert (propertize (format "%-20.20s" (oref data name)) 'face 'font-lock-keyword-face)
          (propertize (format "%-10.10s" (oref data when)) 'face 'font-lock-comment-face)
          (propertize (oref data message) 'face 'italic)))

(cl-defgeneric hermes--item-string (data))
(cl-defmethod hermes--item-string ((data hermes--changeset))
  (car (split-string (oref data rev))))
(cl-defmethod hermes--item-string ((data hermes--shelve))
  (oref data name))
(cl-defmethod hermes--item-string ((data t))
  (oref data file))

(cl-defgeneric hermes--expandable (data))
(cl-defmethod hermes--expandable ((_ hermes--hunk))
  nil)
(cl-defmethod hermes--expandable ((_ t))
  t)

(cl-defgeneric hermes--expand (data node &optional force))
(cl-defmethod hermes--expand ((data hermes--changeset) node &optional force)
  (if (and (oref data files) (not force))
      (dolist (f (oref data files))
        (setf (oref f node)
              (ewoc-enter-after hermes--ewoc node f)))
    (hermes--run-hg-command "Showing revision"
      "status"
      (lambda (o)
        (hermes--parse-status-files data o (oref data rev))
        (hermes--expand data node))
      "--change" (oref data rev))))
(cl-defmethod hermes--expand ((data hermes--file) node &optional force)
  (if (and (oref data hunks) (not force))
      (hermes--show-file-hunks node data)
    (let* ((callback (lambda (o)
                       (setf (oref data hunks)
                             (mapcar (lambda (hunk)
                                       (hermes--hunk
                                        :lines (split-string (concat "@@" hunk) "\n")
                                        :parent data))
                                     (cdr (split-string o "\n@@" t))))
                       (hermes--expand data node)))
           (status (oref data status))
           (filename (oref data file)))
      (cond ((string= "?" status)
             (hermes--async-command "Expanding file"
               "diff"
               callback
               "-u" "/dev/null" filename))
            ((or (not (string= "#" status))
                 (setq filename (hermes--sanitize-filename filename)))
             (hermes--run-hg-command "Expanding file"
               "diff"
               callback
               (when (oref data rev) "--change")
               (when (oref data rev) (oref data rev))
               filename))))))
(cl-defmethod hermes--expand ((data hermes--shelve) node &optional force)
  (if (and (oref data files) (not force))
      (dolist (file (oref data files))
        (setf (oref file node)
              (ewoc-enter-after hermes--ewoc node file)))
    (hermes--run-hg-command "Expanding shelve"
      "shelve"
      (lambda (o)
        (setf (oref data files)
              (mapcar (lambda (f)
                        (let* ((hunks (split-string f "\n@@" t))
                               (file-line (car (split-string (pop hunks) "\n")))
                               (filename (and (string-match " [ab]/\\([^ \\n]+\\)" file-line)
                                              (match-string 1 file-line)))
                               (file (hermes--file :file filename
                                                   :rev nil
                                                   :status "M"
                                                   :parent data)))
                          (setf (oref file hunks)
                                (mapcar (lambda (hunk)
                                          (hermes--hunk
                                           :lines (split-string (concat "@@" hunk) "\n")
                                           :parent file))
                                        hunks))
                          file))
                      (cdr (split-string o "\ndiff --git" t))))
        (hermes--expand data node))
      "-p" (oref data name))))

(cl-defgeneric hermes--visit (data))
(cl-defmethod hermes--visit ((data hermes--changeset))
  (hermes--run-hg-command (format "Updating to %s" (oref data rev))
    "update"
    #'hermes-refresh
    "--rev" (oref data rev)))
(cl-defmethod hermes--visit ((data hermes--file))
  (if (and (string= "#" (oref data status))
           (hermes-resolve--get-status))
      (if-let (file (hermes-resolve--get-current-file))
          (with-current-buffer (funcall (if current-prefix-arg
                                            #'find-file-other-window
                                          #'find-file)
                                        file)
            (revert-buffer nil t))
        (hermes-resolve))
    (funcall (if current-prefix-arg
                 #'find-file-other-window
               #'find-file)
             (oref data file))))
(cl-defmethod hermes--visit ((data hermes--hunk))
  (let ((line-num (get-text-property (point) 'hermes-line-num)))
    (with-current-buffer
        (funcall (if current-prefix-arg
                     #'find-file-other-window
                   #'find-file)
                 (or (hermes--sanitize-filename (oref (oref data parent) file))
                     (oref (oref data parent) file)))
      (when line-num
        (goto-line line-num)))))
(cl-defmethod hermes--visit ((data hermes--shelve))
  (hermes--run-hg-command (format "Unshelve %s" (oref data name))
    "unshelve"
    #'hermes-refresh
    (when current-prefix-arg
      "--keep")
    "-n"
    (oref data name)))

(cl-defgeneric hermes--apply (data &optional reverse))
(cl-defmethod hermes--apply ((data hermes--changeset) &optional reverse)
  (cond ((null (oref data rev))
         (unless reverse
           (error "Cannot apply pending changes"))
         (when (y-or-n-p "Revert pending changes? ")
           (if-let (files (mapcar (lambda (d) (oref d file))
                                  (remove-if-not (lambda (d) (string= "?" (oref d status)))
                                                 (oref data files))))
               (hermes--async-command nil
                 "rm"
                 (lambda (_)
                   (hermes--run-hg-command "Revert pending changes..."
                     "update"
                     #'ignore
                     "-C" "."))
                 "-f" files)
             (hermes--run-hg-command "Revert pending changes..."
               "update"
               #'ignore
               "-C" "."))))
        (t
         (when (y-or-n-p (format (if reverse
                                     "Strip changeset %s? "
                                   "Graft changeset %s? ") (oref data rev)))
           (hermes--run-hg-command (format (if reverse
                                               "Stripping changeset %s..."
                                             "Grafting changeset %s...")
                                           (oref data rev))
             (if reverse "strip" "graft")
             #'ignore
             "--rev" (oref data rev))))))
(cl-defmethod hermes--apply ((data hermes--file) &optional reverse)
  (let ((parent (oref data parent))
        (file (oref data file)))
    (when (hermes--shelve-p parent)
      (error "Cannot %s file under shelve" (if reverse "revert" "apply")))
    (when (and (null reverse)
               (or (string= "?" (oref data status))
                   (null (oref parent rev))))
      (error "Cannot apply this file change"))
    (when (y-or-n-p (format (if reverse "Revert %s? " "Apply %s? ") file))
      (cond ((string= "?" (oref data status))
             (hermes--async-command nil
               "rm"
               #'ignore
               "-f" file))
            ((or (null (oref parent rev))
                 current-prefix-arg)
             (hermes--run-hg-command "Reverting back"
               "revert"
               #'ignore
               (when (oref parent rev) (concat "--rev=" (oref parent rev) "^"))
               "--" file))
            (t
             (hermes--run-hg-command nil
               "diff"
               (lambda (o)
                 (let ((temp-file (make-nearby-temp-file "hunk" nil ".patch")))
                   (write-region o nil temp-file)
                   (hermes--async-command (if reverse "Reverting hunk" "Applying hunk")
                     "patch"
                     (lambda (_)
                       (delete-file temp-file))
                     "--unified" (and reverse "--reverse") "--batch"
                     "--input" temp-file
                     "--" file)))
               "-c" (oref parent rev)
               file))))))
(cl-defmethod hermes--apply ((data hermes--hunk) &optional reverse)
  (when (y-or-n-p (if reverse "Revert hunk? " "Apply hunk? "))
    (let ((temp-file (make-nearby-temp-file "hunk" nil ".patch")))
      (write-region
       (mapconcat #'identity
                  (append (oref data lines) '(""))
                  "\n")
       nil temp-file)
      (hermes--async-command (if reverse "Reverting hunk" "Applying hunk")
        "patch"
        (lambda (_)
          (delete-file temp-file))
        "--unified" (and reverse "--reverse") "--batch"
        "--input" temp-file "--" (oref (oref data parent) file)))))
(cl-defmethod hermes--apply ((data hermes--shelve) &optional reverse)
  (if reverse
      (when (y-or-n-p (format "Delete %s? " (oref data name)))
        (hermes--run-hg-command (format "Deleting shelve %s" (oref data name))
          "shelve"
          #'ignore
          "-d" (oref data name)))
    (when (y-or-n-p (format "Apply %s? " (oref data name)))
      (hermes--run-hg-command (format "Unshelve %s" (oref data name))
        "unshelve"
        #'hermes-refresh
        "--keep"
        "-n"
        (oref data name)))))

;; process invocation
(defvar hermes--async-command-buffer nil)
(put 'hermes--async-command-buffer 'permanent-local t)

(defun hermes-show-last-command ()
  (interactive)
  (and hermes--async-command-buffer
       (buffer-live-p hermes--async-command-buffer)
       (display-buffer hermes--async-command-buffer)))
(defun hermes--process-sentinel (proc event)
  (when (memq (process-status proc) '(exit signal))
    (let ((command-buffer (process-get proc 'command-buffer))
          (callback (process-get proc 'callback))
          (output (with-current-buffer (process-buffer proc)
                    (let ((inhibit-read-only t))
                      (goto-char (marker-position (process-get proc 'start)))
                      (backward-delete-char 3)
                      (prog1
                          (buffer-substring-no-properties (1+ (point))
                                                          (marker-position (process-mark proc)))
                        (put-text-property (process-get proc 'start) (process-mark proc)
                                           'invisible t))))))
      (condition-case err
          (with-current-buffer command-buffer
            (funcall callback output))
        (error
         (message "hermes command error: %s" err))))))
(defun hermes-process-send-string ()
  (interactive)
  (let ((proc (get-text-property (point) 'proc)))
    (unless (and proc (process-live-p proc))
      (error "No active process at point."))
    (let ((m (make-sparse-keymap)))
      (set-keymap-parent m minibuffer-local-map)
      (define-key m (kbd "C-g") #'hermes-process-kill)
      (let ((minibuffer-local-map m))
        (process-send-string proc (concat (read-string "Input: ") "\n"))))))
(defun hermes-process-send-eof ()
  (interactive)
  (let ((proc (get-text-property (point) 'proc)))
    (unless (and proc (process-live-p proc))
      (error "No active process at point."))
    (process-send-eof proc)))
(defun hermes--process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (process-mark proc))
      (insert (propertize string
                          'proc proc
                          'face 'diff-context))
      (set-marker (process-mark proc) (point)))))

(defun hermes-process-quit ()
  (interactive)
  (when (eq (window-buffer) (current-buffer))
    (quit-window current-prefix-arg)))
(defun hermes-process-kill ()
  (interactive)
  (ignore-errors (kill-process (get-text-property (point) 'proc))))
(defun hermes-process-clear ()
  (interactive)
  (let* ((proc (get-text-property (point) 'proc))
         (inhibit-read-only t))
    (unless (and proc (process-live-p proc))
      (let ((inhibit-read-only t))
        (delete-region (1+ (marker-position (process-get proc 'start)))
                       (marker-position (process-mark proc)))))))
(defun hermes-process-toggle-output ()
  (interactive)
  (let* ((proc (get-text-property (point) 'proc))
         (inhibit-read-only t))
    (when proc
      (put-text-property (process-get proc 'start) (process-mark proc)
                         'invisible
                         (not (get-text-property (process-get proc 'start) 'invisible))))))
(defvar hermes-process-output-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-g") #'hermes-process-kill)
    (define-key m (kbd "c") #'hermes-process-clear)
    (define-key m (kbd "n") #'forward-page)
    (define-key m (kbd "p") #'backward-page)
    (define-key m (kbd "q") #'hermes-process-quit)
    (define-key m (kbd "RET") #'hermes-process-send-string)
    (define-key m (kbd "TAB") #'hermes-process-toggle-output)
    (define-key m (kbd "C-d") #'hermes-process-send-eof)
    m))
(defvar-local hermes-process-output-mode nil)
(define-minor-mode hermes-process-output-mode
  "A minor mode for hermes processes."
  (when hermes-process-output-mode
    (buffer-disable-undo)
    (setq buffer-read-only t)))
(defun hermes--async-command (name command callback &rest args)
  "Run command and feed the output to callback.
If more multiple commands are given, runs them in parallel."
  (declare (indent 1))
  (setq args (cl-remove-if-not #'identity args))
  (when (or (null hermes--async-command-buffer)
            (not (buffer-live-p hermes--async-command-buffer)))
    (with-current-buffer
        (setq hermes--async-command-buffer
              (get-buffer-create (concat " CMD " (buffer-name))))
      (hermes-process-output-mode t)))
  (let* ((process-connection-type nil)
         (proc (apply #'start-file-process
                      (or name command)
                      hermes--async-command-buffer
                      command args)))
    (set-process-buffer proc hermes--async-command-buffer)
    (with-current-buffer hermes--async-command-buffer
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (propertize (concat "\n\n"
                                    (mapconcat #'identity (cons command args) " ")
                                    " ...\n")
                            'face 'bold
                            'proc proc)))
      (process-put proc 'start (set-marker (make-marker) (1- (point))))
      (set-marker (process-mark proc) (point)))
    (with-editor-set-process-filter proc #'hermes--process-filter)
    (set-process-sentinel           proc #'hermes--process-sentinel)
    (process-put proc 'command-buffer (current-buffer))
    (process-put proc 'callback callback)
    (when nil
      (with-current-buffer nil
        (process-send-region proc (point-min) (point-max))
        (process-send-eof    proc)))
    proc))

(defun hermes--run-hg-command (name command callback &rest args)
  (declare (indent 1))
  (apply #'hermes--async-command
         name
         (car hermes--hg-commands)
         callback
         (append (cdr hermes--hg-commands)
                 (list command)
                 args)))

(defun hermes--run-interactive-command (name command-and-args &optional callback require-terminal show)
  "Run a command and call callback with the buffer after it is done."
  (declare (indent 1))
  (with-editor
    (apply #'hermes--async-command
           name
           (car command-and-args)
           callback
           (cdr command-and-args)))
  (when show
    (hermes-show-last-command)
    (with-current-buffer hermes--async-command-buffer
      (goto-char (point-max))
      (backward-page 1)
      (narrow-to-region (1+ (point)) (point-max)))))

;; printers
(defvar hermes--ewoc nil)
(put 'hermes--ewoc 'permanent-local t)
(defun hermes--filter-children (data)
  (let ((deleted (list data)))
    (ewoc-filter hermes--ewoc
                 (lambda (d)
                   (not (and d
                             (memq (oref d parent) deleted)
                             (or (setf (oref d expanded) nil)
                                 (push d deleted))))))
    (dolist (d deleted)
      (setf (oref d node) nil))))

(defun hermes--indent (r &optional for-changeset-header)
  (let ((changeset (pcase (type-of r)
                     ('hermes--file (oref r parent))
                     ('hermes--changeset r)
                     (_ nil))))
    (cond ((null changeset)
           "")
          ((hermes--shelve-p changeset)
           "   ")
          ((oref changeset title)
           "   ")
          (t
           (cdr (assq (cond (for-changeset-header 'indent1)
                            ((eq r changeset)     'indent2)
                            (t                    'indent3))
                      (oref changeset props)))))))

(defun hermes-printer (data)
  (if (null data)
      (insert "\n" (make-string (1- (window-width)) ?-))
    (hermes--print data)))

;; parsers
(defun hermes--parse-changesets (o &optional parents)
  "Parse 'hg log' output into hermes--changeset records."
  ;; --debug option may print out some garbage at the beginnig.
  (when-let (p (string-match "\n.\s+changeset: " o))
    (setq o (substring o (1+ p))))
  (let (changesets props)
    (dolist (line (split-string o "\n" t))
      ;; 'o' can be used for graphic representation
      (when (string-match "^\\([^a-np-z]+\\)\\([a-z]+\\): +\\(.*\\)" line)
        (let* ((indent (match-string 1 line))
               (k (intern (match-string 2 line)))
               (v (match-string 3 line))
               p)
          (when (and (eq k 'changeset)
                     props)
            (push (hermes--changeset :rev (cadr (assq 'changeset props))
                                     :summary (cadr (assq 'summary props))
                                     :tags (cdr (assq 'tag props))
                                     :props (nreverse props)
                                     :current (cl-find (cadr (assq 'changeset props))
                                                       parents
                                                       :test #'string=))
                  changesets)
            (setq props nil))
          (push (cons (cond ((eq k 'changeset) 'indent1)
                            ((eq k 'summary)   'indent2)
                            (t                 'indent3))
                      indent)
                props)
          (when (memq k '(changeset parent))
            (setq v (car (nreverse (split-string v ":"))))
            (when (string-match "^0+$" v)
              (setq v nil)))
          (when v
            (if (setq p (assq k props))
                (push v (cdr p))
              (push (cons k (list v)) props))))))
    (when props
      (push (hermes--changeset :rev (cadr (assq 'changeset props))
                               :summary (cadr (assq 'summary props))
                               :tags (cdr (assq 'tag props))
                               :props (nreverse props))
            changesets)
      (setq props nil))
    (hermes--construct-hierarchy (nreverse changesets))))

(defun hermes--construct-hierarchy (changesets)
  (let (trees)
    (dolist (c changesets)
      (push (cons (oref c rev) c) trees))
    (dolist (c changesets)
      (dolist (parent (cdr (assq 'parent (oref c props))))
        (when (and parent (setq parent (cdr (assoc parent trees))))
          (push parent (oref c parent-revs))
          (push c (oref parent child-revs)))))
    (dolist (c changesets)
      (cl-callf nreverse (oref c parent-revs))
      (cl-callf nreverse (oref c child-revs))))
  changesets)

(defun hermes--parse-status-files (parent o &optional rev)
  (setf (oref parent files)
        (mapcar (lambda (line)
                  (hermes--file :file (substring line 2)
                                :rev rev
                                :status (substring line 0 1)
                                :parent parent))
                (split-string (ansi-color-filter-apply o) "\n" t))))

(defun hermes--parse-shelves (o)
  (remove nil (mapcar (lambda (line)
                        (when (string-match "^\\([^ (]+\\) *\\([^)]+)\\) *\\(.*$\\)" line)
                          (hermes--shelve :name (match-string 1 line)
                                          :when (match-string 2 line)
                                          :message (match-string 3 line))))
                      (split-string o "\n" t))))

;; commands
(defun hermes-goto-next (arg)
  "Move to next node."
  (interactive "p")
  (ewoc-goto-next hermes--ewoc arg))

(defun hermes-goto-prev (arg)
  "Move to previous node."
  (interactive "p")
  (ewoc-goto-prev hermes--ewoc arg))

(defun hermes-goto-up-level ()
  "Move to parent node."
  (interactive)
  (let* ((starting-pos (point))
         (node (ewoc-locate hermes--ewoc))
         (data (and node (ewoc-data node)))
         (parent (and node (oref data parent)))
         old-node)
    (setq old-node node
          node (ewoc-goto-prev hermes--ewoc 1))
    (when (eq old-node node)
      (setq node nil))
    (while (not (or (eq old-node node)
                    (eq parent (ewoc-data node))))
      (setq old-node node
            node (ewoc-goto-prev hermes--ewoc 1)))
    (when (eq old-node node)
      (goto-char starting-pos)
      (error "No parent node found."))))

(defun hermes-goto-down-level ()
  "Move to first child node."
  (interactive)
  (let* ((starting-pos (point))
         (node (ewoc-locate hermes--ewoc))
         (data (and node (ewoc-data node))))
    (setq node (ewoc-goto-next hermes--ewoc 1))
    (unless (and node
                 (ewoc-data node)
                 (eq data
                     (oref (ewoc-data node) parent)))
      (goto-char starting-pos)
      (error "No child node found."))))

(defun hermes-goto-same-level-aux (move-fn arg)
  (let* ((starting-pos (point))
         (node (ewoc-locate hermes--ewoc))
         (data (and node (ewoc-data node)))
         (parent (and data (oref data parent)))
         old-node)
    (while (and node (>= (cl-decf arg) 0))
      (setq old-node node
            node (funcall move-fn hermes--ewoc 1))
      (when (eq old-node node)
        (setq node nil))
      (while (and node
                  (setq data (ewoc-data node))
                  (not (eq parent (oref data parent))))
        (setq old-node node
              node (funcall move-fn hermes--ewoc 1))
        (when (eq old-node node)
          (setq node nil))))
    (unless node
      (goto-char starting-pos)
      (error "No more node at the same level."))))

(defun hermes-goto-next-same-level (arg)
  "Move to next node at the same level."
  (interactive "p")
  (hermes-goto-same-level-aux #'ewoc-goto-next arg))

(defun hermes-goto-prev-same-level (arg)
  "Move to previous node at the same level."
  (interactive "p")
  (hermes-goto-same-level-aux #'ewoc-goto-prev arg))

(defun hermes--show-file-hunks (node file)
  (dolist (hunk (oref file hunks))
    (setq node (setf (oref hunk node)
                     (ewoc-enter-after hermes--ewoc node hunk)))))

(defun hermes-toggle-expand ()
  "Expand or shrink current node.
When a prefix argument is given while expanding, recompute childrens."
  (interactive)
  (setq buffer-read-only nil)
  (let* ((node (ewoc-locate hermes--ewoc))
         (data (and node (ewoc-data node)))
         buffer-read-only)
    (when (and data (hermes--expandable data))
      (if (cl-callf not (oref data expanded))
          (hermes--expand data node current-prefix-arg)
        (hermes--filter-children data)))))

(defun hermes--current-data ()
  (when-let (node (ewoc-locate hermes--ewoc))
    (ewoc-data node)))

(defun hermes--current-rev-or-error ()
  (let ((data (hermes--current-data)))
    (while (and data (not (eq (type-of data) 'hermes--changeset)))
      (setq data (oref data parent)))
    (unless data
      (error "Not on a changeset."))
    (oref data rev)))

(defun hermes-visit ()
  "Do appropriate actions on the current node.
Changeset - update to the revision.
File - opens the file. With prefix argument, opens it on other window."
  (interactive)
  (hermes--visit (hermes--current-data)))

(defun hermes-shelve (message)
  "Create shelve.
With prefix argument, keep changes in the working directory."
  (interactive "sShelve message: ")
  (hermes--run-hg-command nil
    "shelve"
    #'hermes-refresh
    "-m" message
    (when current-prefix-arg
      "-k")))

(defun hermes-kill ()
  "Kill current node into the kill ring.
Changeset - revision hash.
Others - filename."
  (interactive)
  (let (buffer-read-only)
    (when-let (data (hermes--current-data))
      (let ((s (hermes--item-string data)))
       (kill-new s)
       (message "Copied %s" s )))))

(defun hermes-apply ()
  "Apply changes under the point."
  (interactive)
  (hermes--apply (hermes--current-data)))

(defun hermes-revert ()
  "Revert changes under the point."
  (interactive)
  (hermes--apply (hermes--current-data) 'reverse))

(defun hermes--current-changeset ()
  (let ((data (hermes--current-data)))
    (while (and data (not (hermes--changeset-p data)))
      (setq data (oref data parent)))
    data))

(defun hermes--all-changeset ()
  (let (changesets)
    (ewoc-map
     (lambda (data)
       (when (and (hermes--changeset-p data) (oref data rev))
         (push data changesets)))
     hermes--ewoc)
    (nreverse changesets)))

(defun hermes-graft ()
  "Graft change under the point to current revision"
  (interactive)
  (let ((rev (oref (hermes--current-changeset) rev)))
    (unless rev
      (error "Not on a revision."))
    (hermes--run-interactive-command (format "Grafting %s" rev)
      `(,@hermes--hg-commands
        "graft" "--rev" ,rev)
      #'hermes-refresh)))

(defun hermes-rebase ()
  "Rebase current revision to the change under the point.
With prefix argument, use the read revision instead of current revision."
  (interactive)
  (let ((dest (oref (hermes--current-changeset) rev)))
    (unless dest
      (error "Not on a revision."))
   (hermes--run-interactive-command (format "Rebase to %s" dest)
     `(,@hermes--hg-commands
       "rebase"
       "--rev" ,(if current-prefix-arg
                    (hermes--read-revision "Revision to rebase: ")
                  ".")
       "--dest" ,dest)
     #'hermes-refresh)))

(defvar hermes-run-hg-history nil)
(defun hermes-run-hg (command)
  "Run arbitrary hg command."
  (interactive "sRun: hg ")
  (hermes--run-interactive-command (car (split-string command nil t))
    `("bash" "-c" ,(concat (mapconcat #'identity hermes--hg-commands " ") command))
    nil nil t))

(defun hermes-addremove ()
  "Add or remove files."
  (interactive)
  (let* ((data (hermes--current-data))
         (changeset (if (hermes--file-p data) (oref data parent) data)))
    (unless (and (hermes--changeset-p changeset)
                 (null (oref changeset rev)))
      (error "Not a pending changes."))
    (hermes--run-hg-command (format "Add/remove files...")
      "addremove"
      #'hermes-refresh
      (unless (eq data changeset)
        (hermes--item-string data)))))

(defun hermes--all-revisions ()
 (let (datas)
    (ewoc-map (lambda (data)
                (when (hermes--changeset-p data)
                  (push data datas)))
              hermes--ewoc)
    datas))

(defun hermes--read-revision (prompt &optional initial-input history)
  (car (split-string (completing-read prompt
                                      (mapcar (lambda (d)
                                                (let ((front (hermes--format-changeset-line d)))
                                                  (concat front
                                                          (make-string (max 0 (- 70 (length front))) ? )
                                                          (oref d summary))))
                                              (remove-if-not #'identity
                                                             (hermes--all-revisions)))
                                      nil
                                      t
                                      initial-input
                                      history))))
(defun hermes-goto-revision (rev)
  "Jump to current revision."
  (interactive (list (hermes--read-revision "Goto revision: ")))
  (ewoc-map (lambda (data)
              (when (and (hermes--changeset-p data)
                         (oref data rev)
                         (string-match (concat "^" (oref data rev)) rev))
                (goto-char (ewoc--node-start-marker (oref data node)))))
            hermes--ewoc))

(defun hermes--head-revision ()
  (let (d)
    (ewoc-map (lambda (data)
                (when (and (hermes--changeset-p data)
                           (oref data current))
                  (setq d (oref data rev))))
              hermes--ewoc)
    d))

(defun hermes-goto-head-revision ()
  "Jump to current revision."
  (interactive)
  (hermes-goto-revision (hermes--head-revision)))

(defun hermes-show-revision ()
  "Show revision details."
  (interactive)
  (let ((data (hermes--current-changeset)))
    (when-let (rev (and (oref data rev)))
      (hermes--run-hg-command (format "showing %s" rev)
        "show"
        (lambda (o)
          (with-current-buffer (get-buffer-create (format "*hermes-show[%s]*" rev))
            (setq buffer-read-only nil)
            (erase-buffer)
            (insert o)
            (goto-char (point-min))
            (diff-mode)
            (view-mode 1)
            (display-buffer (current-buffer))))
        "-g" rev))))

(defun hermes-mark-unmark ()
  "Toggle the mark of a file."
  (interactive)
  (let ((node (ewoc-locate hermes--ewoc))
        (data (hermes--current-data)))
    (when (hermes--hunk-p data)
      (setq data (oref data parent))
      (while (and node (not (eq data (ewoc-data node))) )
        (setq node (ewoc-prev hermes--ewoc node))))
    (cond ((hermes--file-p data)
           (cl-callf not (oref data marked))
           (ewoc-invalidate hermes--ewoc node))
          ((or (hermes--changeset-p data)
               (hermes--shelve-p data))
           (when (and (hermes--expandable data)
                      (not (oref data expanded)))
             (hermes-toggle-expand))
           (mapcar (lambda (file)
                     (cl-callf not (oref file marked)))
                   (oref data files))
           (setq node (ewoc-next hermes--ewoc node))
           (while (and node
                       (ewoc-data node)
                       (eq data (oref (ewoc-data node) parent)))
             (ewoc-invalidate hermes--ewoc node)
             (setq node (ewoc-next hermes--ewoc node)))))))

(defun hermes-unmark-all ()
  "Unmark all files."
  (interactive)
  (dolist (data (hermes--marked-files))
    (cl-callf not (oref data marked)))
  (save-excursion
    (ewoc-refresh hermes--ewoc)))

(defun hermes--marked-files ()
  "Return all marked file objects."
  (let (datas)
    (ewoc-map (lambda (data)
                (cond ((and (hermes--file-p data) (oref data marked))
                       (cl-pushnew data datas :test #'eq))
                      ((and (or (hermes--changeset-p data)
                                (hermes--shelve-p data))
                            (not (oref data expanded)))
                       (mapcar (lambda (file)
                                 (when (and (hermes--file-p file) (oref file marked))
                                   (cl-pushnew file datas :test #'eq)))
                               (oref data files)))))
              hermes--ewoc)
    datas))

(defun hermes--marked-filenames ()
  "Return all marked file names."
  (cl-remove-duplicates
   (mapcar (lambda (data) (oref data file)) (hermes--marked-files))
   :test #'string-equal))

(transient-define-prefix hermes-resolve ()
  "Create a new commit or replace an existing commit."
  [["Resolve"
    ("a" "Abort"         hermes-resolve-abort)
    ("c" "Continue"      hermes-resolve-continue)
    ("m" "Mark resolved" hermes-resolve-mark)]])
(defun hermes--sanitize-filename (filename)
  (when (string-match "^    \\(.*\\)$" filename)
    (match-string 1 filename)))
(defun hermes-resolve--get-status ()
  (let (abort-cmd continue-cmd unresolved-files)
    (ewoc-map (lambda (data)
                (when (and (hermes--changeset-p data)
                           (null (oref data rev)))
                  (dolist (f (oref data files))
                    (when (string= "#" (oref f status))
                      (let ((parts (split-string (oref f file) ": *" t)))
                        (cond ((and (= 2 (length parts))
                                    (string= (car parts) "To abort"))
                               (setq abort-cmd (cdr (split-string (cl-second parts)))))
                              ((and (= 2 (length parts))
                                    (string= (car parts) "To continue"))
                               (setq continue-cmd (cdr (split-string (cl-second parts)))))
                              ((and parts
                                    (setq parts (hermes--sanitize-filename (car parts))))
                               (push parts unresolved-files))))))))
              hermes--ewoc)
    (and abort-cmd (list abort-cmd continue-cmd unresolved-files))))
(defun hermes-resolve-abort ()
  (interactive)
  (let* ((rs (hermes-resolve--get-status))
         (cmd-args (cl-first rs)))
    (unless rs
      (error "Not in conflict resolution state."))
    (hermes--run-interactive-command "Aborting"
      (append hermes--hg-commands cmd-args)
      #'hermes-refresh)))
(defun hermes-resolve-continue ()
  (interactive)
  (let* ((rs (hermes-resolve--get-status))
         (cmd-args (cl-second rs)))
    (unless rs
      (error "Not in conflict resolution state."))
    (when (cl-third rs)
      (error "Cannot continue with unresolved files."))
    (hermes--run-interactive-command "Continuing"
      (append hermes--hg-commands cmd-args)
      #'hermes-refresh)))
(defun hermes-resolve--get-current-file ()
  (let ((data (and (not current-prefix-arg) (hermes--current-data))))
    (and data
         (hermes--file-p data)
         (string= "#" (oref data status))
         (hermes--sanitize-filename (oref data file)))))
(defun hermes-resolve-mark ()
  (interactive)
  (let* ((rs (hermes-resolve--get-status))
         (files (cl-third rs)))
    (unless rs
      (error "Not in conflict resolution state."))
    (let ((file (or (hermes-resolve--get-current-file)
                    (completing-read "File to mark resolved: " files nil t))))
      (with-current-buffer (find-file-noselect file)
        (revert-buffer))
      (hermes--run-interactive-command (format "Marking %s resolved" file)
        (append hermes--hg-commands (list "resolve" "--mark" file))
        #'hermes-refresh))))

(transient-define-prefix hermes-commit ()
  "Create a new commit or replace an existing commit."
  ["Arguments"
   ("-A" "mark new/missing files as added/removed" ("-A" "--addremove"))
   ("-e" "prompt to edit the commit message"       ("-e" "--edit"))
   ("-s" "use the secret phase for committing"     ("-s" "--secret"))
   ("-n" "do not keep empty commit after uncommit" ("-n" "--no-keep"))
   ("-i" "use interactive mode"                    ("-i" "--interactive"))]
  [["Create"
    ("c" "Commit"    hermes-commit-commit)
    ("s" "abSorb"    hermes-commit-absorb)
    ("d" "Duplicate" hermes-commit-duplicate)
    ("u" "Uncommit"  hermes-commit-uncommit)]
   ["Edit HEAD"
    ("a" "Amend"     hermes-commit-amend)]])
(defun hermes-commit-duplicate ()
  "Create a duplicate change."
  (interactive)
  (let ((rev (hermes--current-rev-or-error)))
    (hermes--run-hg-command (format "Duplicating %s" rev)
      "graft"
      #'hermes-refresh
      "-f" "-r" rev)))
(defun hermes-commit-absorb (&optional args)
  (interactive)
  (apply #'hermes--run-hg-command nil
         "absorb"
         #'hermes-refresh
         "--apply-changes"
         (hermes--marked-filenames)))
(defun hermes-commit-uncommit (&optional args)
  "Create a duplicate change."
  (interactive (list (transient-args 'hermes-commit)))
  (apply #'hermes--run-hg-command nil
         "uncommit"
         #'hermes-refresh
         "--allow-dirty-working-copy"
         (hermes--marked-filenames)))
(cl-macrolet ((def (&rest cmds)
                   (let (form)
                     (dolist (cmd cmds)
                       (push `(defun ,(intern (concat "hermes-commit-" cmd)) (&optional args)
                                ,(concat "Run hg " cmd ".")
                                (interactive (list (transient-args 'hermes-commit)))
                                (hermes--run-interactive-command ,cmd
                                  (append hermes--hg-commands (cons ,cmd (append args (hermes--marked-filenames))))
                                  #'hermes-refresh
                                  (member "--interactive" args)))
                             form))
                     (cons 'progn form))))
  (def "commit" "amend"))

(transient-define-prefix hermes-phase ()
  "Set or show the current phase name."
  ["Arguments"
   ("-f" "allow to move boundary backward"         ("-f" "--force"))]
  [["Get"
    ("SPC" "get changeset phase" hermes-phase-show)]
   ["Set"
    ("p" "set changeset phase to public" hermes-phase-public)
    ("d" "set changeset phase to draft"  hermes-phase-draft)
    ("s" "set changeset phase to secret" hermes-phase-secret)]])
(defun hermes-phase-show ()
  "Show the phase of the current changeset."
  (interactive)
  (let* ((rev (hermes--current-rev-or-error)))
    (hermes--run-hg-command nil
      "phase"
      (lambda (o)
        (message "Changeset %s is in %s phase."
                 (propertize (hermes--current-rev-or-error)
                             'face '('font-lock-type-face bold))
                 (propertize (substring (cadr (split-string o ": " t)) 0 -1)
                             'face 'bold)))
      "-r" rev)))
(cl-macrolet ((def (&rest cmds)
                   (let (form)
                     (dolist (cmd cmds)
                       (push `(defun ,(intern (concat "hermes-phase-" cmd)) (&optional args)
                                ,(concat "Run hg phase --" cmd ".")
                                (interactive (list (transient-args 'hermes-phase)))
                                (hermes--run-interactive-command "phase"
                                  (append hermes--hg-commands
                                          (list "phase" ,(concat "--" cmd))
                                          args
                                          (list "-r" (hermes--current-rev-or-error)))))
                             form))
                     (cons 'progn form))))
  (def "public" "draft" "secret"))

;; modes
(defvar hermes-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "g" #'revert-buffer)
    (define-key map "$" #'hermes-show-last-command)
    (define-key map (kbd "TAB") #'hermes-toggle-expand)
    (define-key map (kbd "RET") #'hermes-visit)
    (define-key map "." #'hermes-goto-head-revision)
    (define-key map "j" #'hermes-goto-revision)
    (define-key map "A" #'hermes-addremove)
    (define-key map "c" #'hermes-commit)
    (define-key map "d" #'hermes-show-revision)
    (define-key map "m" #'hermes-mark-unmark)
    (define-key map "u" #'hermes-unmark-all)
    (define-key map ":" #'hermes-run-hg)
    (define-key map "v" #'hermes-phase)
    (define-key map "w" #'hermes-kill)
    (define-key map "z" #'hermes-shelve)
    (define-key map "a" #'hermes-apply)
    (define-key map "k" #'hermes-revert)
    (define-key map "i" #'hermes-graft)
    (define-key map "r" #'hermes-rebase)
    (define-key map "n" #'hermes-goto-next)
    (define-key map "p" #'hermes-goto-prev)
    (define-key map (kbd "M-n") #'hermes-goto-next-same-level)
    (define-key map (kbd "M-p") #'hermes-goto-prev-same-level)
    (define-key map (kbd "C-M-u") #'hermes-goto-up-level)
    (define-key map (kbd "C-M-d") #'hermes-goto-down-level)
    map)
  "Keymap for `hermes-mode'.")

(define-derived-mode hermes-mode special-mode "Hermes"
  "Major mode for *hermes* buffers.

\\{hermes-mode-map}"
  (setq revert-buffer-function #'hermes-refresh)
  (setq buffer-read-only nil)
  (buffer-disable-undo)
  (erase-buffer)
  (set (make-local-variable 'hermes--ewoc)
       (ewoc-create 'hermes-printer (concat "HG repository: "
                                            (abbreviate-file-name
                                             default-directory)
                                            "\n")))
  (setq buffer-read-only t)
  (hl-line-mode 1))

(defun hermes-refresh (&rest _)
  "Refresh *hermes* buffer contents."
  (when (eq major-mode 'hermes-mode)
    (let* ((hermes-buffer (current-buffer))
           (reporter (make-progress-reporter "Refreshing..."))
           (modified (hermes--changeset
                      :title "Pending changes"
                      :rev nil
                      :expanded t))
           recents parents shelves)
      (with-current-buffer hermes-buffer
        (let (buffer-read-only)
          (ewoc-filter hermes--ewoc (lambda (_) nil))))

      (dolist (p (list
                  (hermes--run-hg-command nil
                    "status"
                    (lambda (o) (hermes--parse-status-files modified o)))
                  (hermes--run-hg-command nil
                    "parent"
                    (lambda (o) (setq parents (mapcar (lambda (o) (oref o rev))
                                                      (hermes--parse-changesets o)))))
                  (hermes--run-hg-command nil
                    "shelve"
                    (lambda (o) (setq shelves (hermes--parse-shelves o)))
                    "--list")
                  (hermes--run-hg-command nil
                    "log"
                    (lambda (o) (setq recents (hermes--parse-changesets o)))
                    "--debug" "-G"
                    "-T" hermes--log-template
                    "-r" hermes--log-revset)))
        (while (process-live-p p)
          (sleep-for 0.05)))
      (dolist (changeset recents)
        (when (cl-find (oref changeset rev) parents :test #'string=)
          (setf (oref changeset current) t)))
      (with-current-buffer hermes-buffer
        (let (buffer-read-only)
          (when (oref modified files)
            (hermes--expand modified (setf (oref modified node)
                                           (ewoc-enter-last hermes--ewoc modified)))
            (ewoc-enter-last hermes--ewoc nil))
          (when recents
            (dolist (changeset recents)
              (setf (oref changeset node)
                    (ewoc-enter-last hermes--ewoc changeset)))
            (when shelves
              (ewoc-enter-last hermes--ewoc nil)))
          (when shelves
            (dolist (shelve shelves)
              (setf (oref shelve node)
                    (ewoc-enter-last hermes--ewoc shelve))))))
      (progress-reporter-done reporter))))

(defun hermes-read-root-dir ()
  "read root dir"
  (let ((d (vc-find-root default-directory ".hg")))
    (when (or current-prefix-arg (null d))
      (setq d (vc-find-root
               (read-directory-name "HG repository directory: ") ".hg")))
    (list d)))
;;;###autoload (autoload 'hermes "hermes" nil t)
(defun hermes (&optional directory)
  "Starts a *hermes* buffer on current directory."
  (interactive (hermes-read-root-dir))
  (unless (or directory
              (setq directory (vc-find-root default-directory ".hg")))
    (error "No HG repository found!"))
  (let* ((rpaths (nreverse (split-string directory "/" t)))
         (name (pop rpaths))
         (refresh (>= (prefix-numeric-value current-prefix-arg) 16))
         buffer)
    (dolist (b (buffer-list))
      (when (and (null buffer)
                 (eq 'hermes-mode (buffer-local-value 'major-mode b))
                 (string= directory (buffer-local-value 'default-directory b)))
        (setq buffer b)))
    (while (and (null buffer)
                (get-buffer (format "*hermes[%s]*" name))
                (not (string= directory
                              (buffer-local-value 'default-directory
                                                  (get-buffer (format "*hermes[%s]*" name))))))
      (setq name (concat name "|" (pop rpaths))))
    (let ((default-directory directory))
      (with-current-buffer (or buffer (get-buffer-create (format "*hermes[%s]*" name)))
        (display-buffer (current-buffer))
        (unless (derived-mode-p 'hermes-mode)
          (setq refresh t)
          (hermes-mode))
        (when refresh
          (hermes-refresh))))))

(provide 'hermes)
;;; hermes.el ends here
