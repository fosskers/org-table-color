;;; org-table-colour.el --- Add colour to your org-mode table cells -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Colin Woodbury
;;
;; Author: Colin Woodbury <colin@fosskers.ca>
;; Maintainer: Colin Woodbury <colin@fosskers.ca>
;; Created: March 10, 2022
;; Modified: March 10, 2022
;; Version: 0.0.1
;; Keywords: data faces lisp
;; Homepage: https://github.com/fosskers/org-table-colour
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Add colour to your org-mode table cells.
;;
;;; Code:

(require 'org-table)

;;;###autoload
(defun org-table-colour (get-face)
  "Colour the 'org-mode' table at 'point', given a GET-FACE function.

GET-FACE must accept a single numerical argument (the value of
the cell) and return either a plist representing a face or nil.
When nil, no styling of that cell will occur. Further, no styling
will occur if the cell value is not a number.

See `org-table-colour--colour-by-correlation' for an example."
  (let* ((lisp (org-table-to-lisp))
         (rows (length lisp))
         (cols (length (car lisp))))
    (save-excursion
      (mapc (lambda (x) (mapc (lambda (y) (org-table-colour--colour-cell get-face x y))
                              (number-sequence 2 rows)))
            (number-sequence 2 cols)))))

;;;###autoload
(defun org-table-colour-correlation-matrix ()
  "Colour the 'org-mode' table at 'point' that represents a Correlation Matrix."
  (interactive)
  (org-table-colour #'org-table-colour--colour-by-correlation))

(defun org-table-colour--colour-cell (get-face x y)
  "Colour the cell via a GET-FACE function at the given X and Y coordinates."
  (org-table-goto-line y)
  (org-table-goto-column x)
  (when-let* ((cell (org-table-get y x))
              (nmbr (string-to-number cell))
              (face (funcall get-face nmbr))
              (over (make-overlay (point)
                                  (progn (org-table-end-of-field 1)
                                         (point)))))
    (overlay-put over 'face face)))

(defun org-table-colour--colour-by-correlation (num)
  "Colour a table cell NUM value assuming it's from a correlation matrix.
Yields a plist that represents a face."
  (cond ((>= num 0.5) '(:foreground "black" :background "green"))
        ((>= num 0.3) '(:foreground "black" :background "#90EE90"))
        ((<= num -0.5) '(:foreground "black" :background "red"))
        ((<= num -0.3) '(:foreground "black" :background "orange"))))

(provide 'org-table-colour)
;;; org-table-colour.el ends here
