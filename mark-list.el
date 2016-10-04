;;; mark-list.el --- mark postions and go back later  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  zaijian

;; Author: zaijian <zaijian@zaijiandeMacBook-Pro.local>
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4") (ivy "20160913.535"))
;; Version: 20161005.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'ivy)

(define-namespace mark-list/		;namespace


(defvar /mark-list-max-length 20
  "max num of mark in the list.")

(defvar /mark-list
  ()
  "mark list")


(defun /make-mark (origin-content pos buffer &optional desc)
  "create a mark.
mark looks like this：
#(\" line content \" 
0 1 (buffer-name \"xxx\" char-pos \"1\" origin-content \"origin content\")
80 81 (display \"desc, buffer-name\" )
)
length of line content is less then 70.
"
  (let ((format-content (format "%-70.70s            " origin-content)))
    (put-text-property 0 1 'buffer-name buffer format-content)
    (put-text-property 0 1 'char-pos pos format-content)
    (put-text-property 0 1 'origin-content origin-content format-content)
    (put-text-property 80 81 'display (format "[%s]<%s>" (if desc desc "") buffer) format-content)
    (put-text-property 81 82 'display "" format-content)
    format-content
    )
  )

;;;###autoload
(defun mark-here (desc)
  "make a mark at point"
  (interactive
   (list
    (if current-prefix-arg
	(read-from-minibuffer "desc: ")
      nil)))
  (let* ((line-begin (line-beginning-position))
	 (line-end (line-end-position))
	 (line-content (buffer-substring-no-properties line-begin line-end)))
    (setq /mark-list
	  (cons (/make-mark line-content (point) (buffer-name) desc)
		(if (>= (length /mark-list) /mark-list-max-length)
		    (cdr /mark-list)
		  /mark-list)))
    )
  )


(defun /update-modify-flag-in-mark (mark)
  (if (/modified? mark)
      (put-text-property 81 82 'display "[M]" mark)
    (put-text-property 81 82 'display "" mark))
  )

(defun /candidate-with-check-modified ()
  (cl-mapc #'/update-modify-flag-in-mark /mark-list))

;;;###autoload
(defun show-marks ()
  "show mark list. "
  (interactive)
  (let ((origin-buffer (buffer-name))
	(origin-pos (point))
	res)
    (unwind-protect
	(setq res
	      (ivy-read "marks: "
			(/candidate-with-check-modified)
			:require-match t
			;; :initial-input last-chosen-mark
			:update-fn #'/update-input-ivy
			:action #'/action-ivy
			:caller "mark-list/show-marks"
			))
      (if (not res)
	  (progn
	    (switch-to-buffer origin-buffer)
	    (goto-char origin-pos))
	(/goto-char-pos res))
      ))
  )


(defun /action-ivy (a)
  "treat the last chosen mark as 1st one."
  (setq /mark-list
	(cons a (cl-remove a /mark-list :test 'equal-including-properties)))
  )

(defun /update-input-ivy ()
  "called when `ivy' input is updated."
  (with-ivy-window
    (save-excursion
      (let* ((buffer-name (get-text-property 0 'buffer-name ivy--current))
	     ;; (char-pos-aux (get-text-property 0 'char-pos ivy--current))
	     ;; (char-pos (if (stringp char-pos-aux)
	     ;; 		   (string-to-int char-pos-aux)
	     ;; 		 char-pos-aux))
	     )

	(if (not (get-buffer buffer-name))
	    nil				;if buffer not opened, just skip it
	  (/goto-char-pos ivy--current)
	  )
	)
      )
    )
  )

(defun /goto-char-pos (mark)
  (let ((buffer (get-text-property 0 'buffer-name mark))
	(char-pos (get-text-property 0 'char-pos mark)))
    (switch-to-buffer buffer)
    (goto-char char-pos)
    )
  )

(defun /modified? (mark)
  ;;MARK here is not normal mark in emacs,
  ;;but a string with properties,
  ;;see also in comment of `mark-list//make-mark'
  (let ((current-buffer (get-buffer (get-text-property 0 'buffer-name mark))))
    (if (not current-buffer)
	nil
      (with-current-buffer current-buffer
	(save-excursion
	  (let ((origin-pos (get-text-property 0 'char-pos mark)))
	    (goto-char origin-pos)
	    (let ((current-line-content
		   (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position))))
	      (not (equal current-line-content (get-text-property 0 'origin-content mark))))
	    ))))))


)

(provide 'mark-list)
;;; mark-list.el ends here






