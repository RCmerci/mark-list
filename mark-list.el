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

(eval-when-compile (require 'names))


(define-namespace mark-list/		;namespace


(defvar /mark-list-max-length 20
  "max num of mark in the list.")

(defvar /mark-list
  ()
  "mark list")

(defvar /overlay-stack nil
  "overlay stack")

(defvar handle-candidate-hook nil
  "setup candidates before showing marks.
if function in hook return nil, drop this candidate;
                    return t  , keep it;
                    return mark, change origin one to new mark.
")


(defun /make-mark (origin-content pos buffer &optional desc)
  "create a mark.
mark looks like this：
#(\" line content \" 
0 1 (buffer-name \"xxx\" char-pos \"1\" origin-content \"origin content\")
80 81 (display \"desc, buffer-name\" )
)
length of line content is less then 70.
"
  (let ((format-content (format "%-70.70s            " (replace-regexp-in-string "\t" "    " origin-content))))
    (put-text-property 0 1 'buffer-name buffer format-content)
    (put-text-property 0 1 'char-pos pos format-content)
    (put-text-property 0 1 'origin-content origin-content format-content)
    (put-text-property 80 81 'display (format "[%s]<%s>" (if desc desc "") buffer) format-content)
    (put-text-property 81 82 'display "" format-content)
    format-content
    )
  )






(defun /update-modify-flag-in-mark (mark)
  (if (/modified? mark)
      (put-text-property 81 82 'display "[M]" mark)
    (put-text-property 81 82 'display "" mark))
  t
  )

(defun /killed-buffer-filter (mark)
  (if (get-buffer (/get buffer-name mark))
      t
    nil))


(defun /handle-mark (mark handlers)
  (if (null handlers)
      mark
    (let ((handler (car handlers)))
      (if mark
	  (let ((res (funcall handler mark)))
	  (if (equal t res)
	      (/handle-mark mark (cdr handlers))
	    (if (stringp res)
		(/handle-mark res (cdr handlers))
	      nil)))
	nil
	))))


(defun /get-candidates ()
  (let ((res-mark-list nil))
    (dolist (mark (reverse /mark-list))
      (let ((handled-mark (/handle-mark mark handle-candidate-hook)))
	(message handled-mark)
	(when handled-mark
	  (push handled-mark res-mark-list))
	))
    res-mark-list
    )
  )






(defun /action-ivy (a)
  "treat the last chosen mark as 1st one."
  (/clear-overlay)
  (setq /mark-list
	(cons a (cl-remove a /mark-list :test 'equal-including-properties)))
  )


(defun /update-input-ivy ()
  "called when `ivy' input is updated."
  (/clear-overlay)
  (with-ivy-window
    (let ((buffer-name (/get buffer-name ivy--current)))
      (when (and (< 0 (length buffer-name))
		 (get-buffer buffer-name))
	(/goto-char-pos ivy--current)
	(let ((ov (/make-overlay
		   (line-beginning-position)
		   (1+ (line-end-position)))))
	  (overlay-put ov 'face 'highlight)
	  )
	)
      )
    )
  )

(defun /make-overlay (beg end)
  (let ((ov (make-overlay beg end)))
    (setq /overlay-stack (cons ov /overlay-stack))
    ov)
  )



(defun /clear-overlay ()
  (dolist (ov /overlay-stack)
    (delete-overlay ov))
  (setq /overlay-stack nil)
  )

(defun /delete-mark (mark)
  (setq /mark-list
	(cl-remove-if (lambda (m)
			(/mark-equal m mark))
		      /mark-list
		      :count 1))
    
  )


;;; ================utils================
(defmacro /get (property mark)
  "
(/get buffer-name m)
(/get char-pos m)
"
  `(get-text-property 0 ',property ,mark))


(defun /mark-equal (m1 m2)
  (and
   (equal
    (/get buffer-name m1)
    (/get buffer-name m2)
    )
   (equal
    (/get char-pos m1)
    (/get char-pos m2))
   (equal
    (/get origin-content m1)
    (/get origin-content m2)))
  )



(defun /goto-char-pos (mark)
  (let ((buffer (/get buffer-name mark))
	(char-pos (/get char-pos mark)))
    (switch-to-buffer buffer)
    (goto-char char-pos)
    )
  )

(defun /modified? (mark)
  ;;MARK here is not normal mark in emacs,
  ;;but a string with properties,
  ;;see also in comment of `mark-list//make-mark'
  (let ((current-buffer (get-buffer (/get buffer-name mark))))
    (if (not current-buffer)
	nil
      (with-current-buffer current-buffer
	(save-excursion
	  (let ((origin-pos (/get char-pos mark)))
	    (goto-char origin-pos)
	    (let ((current-line-content
		   (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position))))
	      (not (equal current-line-content (/get origin-content mark))))
	    ))))))

(defun /buffer-exist? (mark)
  (let ((buffer-name (/get buffer-name mark)))
    (and (< 0 (length buffer-name))
	 (get-buffer buffer-name)) 
    )
  )

)


;;;###autoload
(defun mark-list/mark-here (desc)
  "make a mark at point"
  (interactive
   (list
    (if current-prefix-arg
	(read-from-minibuffer "desc: ")
      nil)))
  (let* ((line-begin (line-beginning-position))
	 (line-end (line-end-position))
	 (line-content (buffer-substring-no-properties line-begin line-end)))
    (setq mark-list//mark-list
	  (cons (mark-list//make-mark line-content (point) (buffer-name) desc)
		(if (>= (length mark-list//mark-list) mark-list//mark-list-max-length)
		    (cdr mark-list//mark-list)
		  mark-list//mark-list)))
    (message "mark here")
    )
  )


;;;###autoload
(defun mark-list/show-marks ()
  "show mark list. "
  (interactive)
  (let ((origin-buffer (buffer-name))
	(origin-pos (point))
	res)
    (unwind-protect
	(setq res
	      (ivy-read "marks: "
			(mark-list//get-candidates)
			;; (/candidate-with-check-modified)
			:require-match t
			;; :initial-input last-chosen-mark
			:update-fn #'mark-list//update-input-ivy
			:action #'mark-list//action-ivy
			:caller "mark-list/show-marks"
			))
      (progn
	(mark-list//clear-overlay)
	(if (not res)
	    (progn
	      (switch-to-buffer origin-buffer)
	      (goto-char origin-pos))
	  (if (mark-list//buffer-exist? res)
	      (mark-list//goto-char-pos res)
	    (message (format "buffer: <%s> has been killed" (mark-list//get buffer-name res))))))
      ))
  )




(when (null mark-list/handle-candidate-hook) ;; init hook
    (add-hook 'mark-list/handle-candidate-hook #'mark-list//update-modify-flag-in-mark)
    (add-hook 'mark-list/handle-candidate-hook #'mark-list//killed-buffer-filter )
  )

(ivy-set-actions
 'mark-list/show-marks
 '(("d" mark-list//delete-mark "delete mark")))







(provide 'mark-list)
;;; mark-list.el ends here






