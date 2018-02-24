;;; grep-context.el --- TODO: Write short summary.  -*- lexical-binding: t; -*-

;;
;; Author: Michał Kondraciuk <k.michal@zoho.com>
;; URL: https://github.com/mkcms/grep-context
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.0.1
;; Keywords: convenience, tools, search, grep, compile

;;; Commentary:
;;
;; Long project summary.
;; TODO: Write long summary.

(require 'compile)
(require 'dash)

(eval-when-compile
  (require 'subr-x))

(defgroup grep-context nil
  "TODO: Write group documentation."
  :group 'compilation
  :group 'grep)

(defcustom grep-context-line-format-alist
  (list (cons 'grep-mode "%s-%d-"))
  "Alist that associates major modes with line formatters.
Each value is a string passed to `format' to format a line of context.
It should contain two %-sequences, for the file and a line number in that file,
e.g. \"%s:%d:\".
Value can also be a function callable with a filename and a line number
and should return a formatted prefix string."
  :type '(alist :key-type (symbol :tag "Major mode")
		:value-type (choice string function))
  :group 'grep-context)

(defcustom grep-context-separator-alist
  (list (cons 'grep-mode "--"))
  "Alist that associates major modes with separators.
Each value is a string that's inserted between non-contiguous regions.
If an entry is missing for a major mode, separators are not used in that mode."
  :type '(alist :key-type (symbol :tag "Major mode")
		:value-type (choice string (const :tag "No separator" nil)))
  :group 'grep-context)

(defcustom grep-context-default-format "%s:%d:"
  "Default format for context lines.
Used if `grep-context-line-format-alist' contains no entry for current major
mode."
  :type '(choice string function)
  :group 'grep-context)

(defvar grep-context--temp-file-buffer nil
  "A cell (file . buffer) where BUFFER is a buffer with contents of FILE.")

(defun grep-context--match-location (&optional n)
  "In current compilation buffer, get location for match at point.
If N is non-nil, call `compilation-next-error' with N as argument first.
Return value is a cell (file . line)."
  (save-excursion
    (let* ((msg (compilation-next-error (or n 0)))
	   (loc (compilation--message->loc msg))
	   (fs (compilation--loc->file-struct loc))
	   (file (car (compilation--file-struct->file-spec fs)))
	   (line (compilation--loc->line loc)))
      (cons file line))))

(defun grep-context--at-match (&optional n)
  "Get number of lines of context around match at point.
If N is non-nil, call `compilation-next-error' with N as argument first.
Return value is a cell (context-before . context-after) that can be modified."
  (save-excursion
    (compilation-next-error (or n 0))
    (or (get-text-property (point) 'grep-context)
	(let ((cell (cons 0 0))
	      (inhibit-read-only t))
	  (put-text-property (point-at-bol) (point-at-eol) 'grep-context cell)
	  cell))))

(defun grep-context--format-line (format file line-number line)
  (if (stringp format)
      (concat (format format file line-number) line)
    (concat (funcall format file line-number) line)))

(defun grep-context-more-around-point (&optional n)
  "Increase context around point by N.
If N is negative, remove -N lines of context.
N defaults to 1."
  (interactive "p")
  (unless (compilation-buffer-p (current-buffer))
    (error "Current buffer is not compilation buffer"))

  (or n (setq n 1))
  (-let* (((file . line) (grep-context--match-location))
	  (ctx (grep-context--at-match))

	  ;; File, line, context around previous/next match
	  ((prev-file . prev-line) (ignore-errors
				     (grep-context--match-location -1)))
	  ((next-file . next-line) (ignore-errors
				     (grep-context--match-location 1)))
	  ((_ . prev-ctx) (ignore-errors (grep-context--at-match -1)))
	  ((next-ctx . _) (ignore-errors (grep-context--at-match 1)))

	  ;; Number of lines that can be inserted before/after match at point
	  (avail-before
	   (min n (or (and (equal file prev-file) (< prev-line line)
			   (- line 1 (car ctx) (+ prev-line prev-ctx)))
		      n)))
	  (avail-after
	   (min n (or (and (equal file next-file) (< line next-line)
			   (- next-line 1 next-ctx (+ line (cdr ctx))))
		      n)))

	  (format (or (cdr (assoc major-mode grep-context-line-format-alist))
		      grep-context-default-format))
	  (separator (cdr (assoc major-mode grep-context-separator-alist)))
	  (buffer (current-buffer))
	  (inhibit-read-only t))

    ;; Remove separator before and after this match
    (dolist (line-outside (list (1+ (cdr ctx)) (- (1+ (car ctx)))))
      (save-excursion
	(forward-line line-outside)
	(when (get-text-property (point) 'grep-context-separator)
	  (kill-whole-line))))

    (if (< n 0)
	(progn
	  (let ((n n))
	    (save-excursion
	      (forward-line (- (car ctx)))
	      (while (and (<= (cl-incf n) 0) (> (car ctx) 0))
		(kill-whole-line)
		(cl-decf (car ctx)))))
	  (let ((n n))
	    (save-excursion
	      (forward-line (cdr ctx))
	      (while (and (<= (cl-incf n) 0) (> (cdr ctx) 0))
		(kill-whole-line -1)
		(cl-decf (cdr ctx))))))

      ;; Prepare a buffer with file contents.
      ;; It's cached so next calls to this function will be faster.
      (unless (and grep-context--temp-file-buffer
		   (equal (car grep-context--temp-file-buffer) file))
	(when (buffer-live-p (cdr grep-context--temp-file-buffer))
	  (kill-buffer (cdr grep-context--temp-file-buffer)))
	(with-current-buffer
	    (generate-new-buffer (generate-new-buffer-name " *tempbuffer*"))
	  (insert-file-contents file)
	  (setq grep-context--temp-file-buffer (cons file (current-buffer)))))

      (with-current-buffer (cdr grep-context--temp-file-buffer)
	(goto-char (point-min))
	(unless (= (forward-line (1- line)) 0)
	  (error "Line %s is out of bounds for this file" line))

	;; Insert context lines before
	(save-excursion
	  (forward-line (- (car ctx)))

	  (while (and (>= (cl-decf avail-before) 0) (= (forward-line -1) 0))
	    (let ((string (buffer-substring (point-at-bol) (point-at-eol))))
	      (with-current-buffer buffer
		(forward-line (- (car ctx)))
		(beginning-of-line)
		(open-line 1)
		(insert (grep-context--format-line
			 format file (- line 1 (car ctx)) string))
		(cl-incf (car ctx))
		(forward-line (car ctx))))))

	;; Insert context lines after
	(save-excursion
	  (forward-line (cdr ctx))

	  (while (and (>= (cl-decf avail-after) 0) (= (forward-line 1) 0))
	    (let ((string (buffer-substring (point-at-bol) (point-at-eol))))
	      (with-current-buffer buffer
		(save-excursion
		  (forward-line (1+ (cdr ctx)))
		  (beginning-of-line)
		  (insert (grep-context--format-line
			   format file (+ line 1 (cdr ctx)) string))
		  (open-line 1)
		  (cl-incf (cdr ctx)))))))))

    ;; Insert separator before and after this match
    (when separator
      (unless (or (and (equal file prev-file) (< prev-line line)
		       (= (+ prev-line prev-ctx (car ctx) 1) line))
		  (and (= (car ctx) 0) (or (null prev-ctx) (= prev-ctx 0))))
	(forward-line (- (car ctx)))
	(beginning-of-line)
	(open-line 1)
	(insert (propertize separator 'grep-context-separator t))
	(forward-line (1+ (car ctx))))
      (unless (or (and (equal file next-file) (< line next-line)
		       (= (+ line (cdr ctx) next-ctx 1) next-line))
		  (and (= (cdr ctx) 0) (or (null next-ctx) (= next-ctx 0))))
	(save-excursion
	  (forward-line (1+ (cdr ctx)))
	  (beginning-of-line)
	  (open-line 1)
	  (insert (propertize separator 'grep-context-separator t)))))))

(defun grep-context-less-around-point (&optional n)
  "Decrease context around POINT by N.
N defaults to 1."
  (interactive "p")
  (grep-context-more-around-point (- (or n 1))))

(provide 'grep-context)

;;; grep-context.el ends here
