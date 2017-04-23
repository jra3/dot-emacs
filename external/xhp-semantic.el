;;; xhp-semantic.el --- util functions for getting semantic information about xhp code

;; Copyright (C) 2013  Aaron Brady

;; Author: Aaron Brady <abrady@dev1735.prn1.facebook.com>
;; Keywords: languages, convenience

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

(defun xhp-semantic-get-class-for-point ()
  "helper to get the class the point is in. should probably stick this "
  (save-excursion
    (and
     (re-search-backward "^\\(?:abstract \\|final \\)*class \\(.*?\\)\\( \\|$\\)" nil t)
     (match-string 1))))

(provide 'xhp-semantic)
;;; xhp-semantic.el ends here
