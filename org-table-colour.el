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
(defun org-table-colour-table ()
  "Colour the 'org-mode' table at 'point'."
  (interactive)
  (let* ((lisp (org-table-to-lisp))
         (rows (length lisp))
         (cols (length (car lisp))))
    (save-excursion
      (mapc (lambda (x) (mapc (lambda (y) (org-table-colour--colour-cell x y))
                              (number-sequence 2 rows)))
            (number-sequence 2 cols)))))

(defun org-table-colour--colour-cell (x y)
  "Colour the cell at the given X and Y coordinates."
  (org-table-goto-line y)
  (org-table-goto-column x)
  (when-let* ((cell (org-table-get y x))
              (nmbr (string-to-number cell))
              (face (cond ((>= nmbr 0.5) '(:foreground "black" :background "green"))
                          ((>= nmbr 0.3) '(:foreground "black" :background "#90EE90"))
                          ((<= nmbr -0.5) '(:foreground "black" :background "red"))
                          ((<= nmbr -0.3) '(:foreground "black" :background "orange")))))
    (when face
      (let ((overlay (make-overlay (point)
                                   (progn (org-table-end-of-field 1) (point)))))
        (overlay-put overlay 'face face)))))

(provide 'org-table-colour)
;;; org-table-colour.el ends here
