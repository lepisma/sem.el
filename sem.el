;;; sem.el --- Semantic indexing and search -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29"))
;; Keywords: tools, search, ml
;; URL: https://github.com/lepisma/sem.el

;;; Commentary:

;; Semantic indexing and search
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'sem-core)

(defcustom sem-database-dir nil
  "Directory where sem will keep all indices and data."
  :type 'string)

(defun sem-store-new (name emb-size)
  "Create a new sem store NAME with vectors of dim EMB-SIZE."
  (if (not sem-database-dir)
      (error "`sem-database-dir' is not set")
    (sem-core-store-new sem-database-dir name emb-size)))

(defun sem-store-load (name)
  "Load an existing store NAME."
  (if (not sem-database-dir)
      (error "`sem-database-dir' is not set")
    (sem-core-store-load sem-database-dir name)))

(defun sem-add (store item embed-fn &optional write-fn)
  "Add given ITEM to the STORE.

WRITE-FN defaults to `prin1' and is used for serialization.  EMBED-FN is
used to convert the item, directly, to vector."
  (sem-core-add sem-database-dir store (funcall (or write-fn #'prin1) item) (funcall embed-fn item)))

(defun sem-similar (store item k embed-fn &optional read-fn)
  "Return K items similar to ITEM from STORE.

EMBED-FN is used to convert the item, directly, to vector.  READ-FN
defaults to `read' and is used to recover the emacs-lisp object back
from the store."
  (let ((results (sem-core-similar store (funcall embed-fn item) k)))
    (mapcar (lambda (it) (cons (car it) (funcall (or read-fn #'read) (cdr it)))) results)))

(defun sem-item-present-p (store item &optional write-fn)
  "Return nil if ITEM is not present in STORE, else return the index.

WRITE-FN defaults to `prin1' and is used for serialization."
  (sem-core-item-present-p store (funcall (or write-fn #'prin1) item)))

(provide 'sem)

;;; sem.el ends here
