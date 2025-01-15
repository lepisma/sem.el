;;; sem.el --- Semantic indexing and search -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.1.0
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

(defun sem-store--path (name)
  "Return path on the file system for the given store NAME."
  (concat (file-name-as-directory sem-database-dir) name))

(defun sem-store-present-p (name)
  "Return whether a store with NAME is already present."
  (if (not sem-database-dir)
      (error "`sem-database-dir' is not set")
    (file-exists-p (sem-store--path name))))

(defun sem-store-new (name emb-size)
  "Create a new sem store NAME with vectors of dim EMB-SIZE."
  (if (not sem-database-dir)
      (error "`sem-database-dir' is not set")
    (if (sem-store-present-p name)
        (error "Store %s already exists" name)
      (sem-core-store-new sem-database-dir name emb-size))))

(defun sem-store-load (name)
  "Load an existing store NAME and return."
  (if (not sem-database-dir)
      (error "`sem-database-dir' is not set")
    (if (sem-store-present-p name)
        (sem-core-store-load sem-database-dir name)
      (error "Store %s doesn't exist" name))))

(defun sem-store-delete (name)
  "Delete store NAME."
  (delete-directory (sem-store--path name) t))

(defun sem-add-batch (store items embed-fn &optional write-fn)
  "Add given ITEMS (list) to the STORE.

WRITE-FN defaults to `prin1' and is used for serialization.  EMBED-FN is
used to convert the list of items to 2D vector."
  (let ((contents (apply #'vector (mapcar (lambda (item) (funcall (or write-fn #'prin1) item)) items)))
        (embeddings (funcall embed-fn items)))
    (sem-core-add-batch store contents embeddings)))

(defun sem-similar (store item k embed-fn &optional read-fn)
  "Return K items similar to ITEM from STORE.

EMBED-FN is used to convert a list of items to 2D vector.  READ-FN
defaults to `read' and is used to recover the emacs-lisp object back
from the store."
  (let ((results (sem-core-similar store (aref (funcall embed-fn (list item)) 0) k)))
    (mapcar (lambda (it) (cons (car it) (funcall (or read-fn #'read) (cdr it)))) results)))

(provide 'sem)

;;; sem.el ends here
