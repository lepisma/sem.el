;;; sem.el --- Semantic indexing and search -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.3.1
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

(require 'cl-macs)

(cl-eval-when (load eval)
  (unless (require 'sem-core nil t)
    (if (or noninteractive
            (yes-or-no-p "Module sem-core must be built.  Do so now? "))
        (let ((default-directory (file-name-directory (or load-file-name
                                                          buffer-file-name)))
              (build-command "make release"))

          (message "Building sem-core module with %S" build-command)
          (with-temp-buffer
            (unless (zerop (call-process-shell-command build-command nil t t))
              (error "Failed to compile module sem-core: %s" (buffer-substring-no-properties (point-min) (point-max)))))
          (message "Loading sem-core")
          (require 'sem-core))
      (user-error "Abort compilation for sem-core"))))

(defcustom sem-data-dir nil
  "Directory where sem will keep all data and indices."
  :type 'string)

(defcustom sem-default-table-name "main"
  "Name of the default table to be created in any sem database."
  :type 'string)

(defun sem-db--path (name)
  "Return path on the file system for the given db NAME."
  (concat (file-name-as-directory sem-data-dir) name))

(defun sem-db-present-p (name)
  "Return whether a db with NAME is already present."
  (if (not sem-data-dir)
      (error "`sem-data-dir' is not set")
    (file-exists-p (sem-db--path name))))

(defun sem-db-new (name dim)
  "Create a new sem db NAME with default table of dimension DIM."
  (if (not sem-data-dir)
      (error "`sem-data-dir' is not set")
    (if (sem-db-present-p name)
        (error "Database %s already exists" name)
      (let ((db (sem-core-db-new sem-data-dir name)))
        (sem-table-new db sem-default-table-name dim)))))

(defun sem-db-load (name)
  "Load an existing db NAME and return."
  (if (not sem-data-dir)
      (error "`sem-data-dir' is not set")
    (if (sem-db-present-p name)
        (sem-core-db-load sem-data-dir name)
      (error "Database %s doesn't exist" name))))

(defun sem-db-delete (name)
  "Delete db NAME."
  (delete-directory (sem-db--path name) t))

(defalias 'sem-table-new #'sem-core-table-new)
(defalias 'sem-table-present-p #'sem-core-table-present-p)
(defalias 'sem-table-delete #'sem-core-table-delete)
(defalias 'sem-table-dim #'sem-core-table-dim)

(defun sem-add-batch (db items embed-batch-fn &optional write-fn)
  "Add given ITEMS (list) to DB.

WRITE-FN defaults to `prin1-to-string' and is used for serialization.
EMBED-BATCH-FN is used to convert the list of items to 2D matrix."
  (let ((contents (apply #'vector (mapcar (lambda (item) (funcall (or write-fn #'prin1-to-string) item)) items)))
        (embeddings (funcall embed-batch-fn items)))
    (sem-core-add-batch db sem-default-table-name contents embeddings)))

(defun sem-add (db item embed-fn &optional write-fn)
  "Add one ITEM to the DB.

WRITE-FN defaults to `prin1-to-string' and is used for serialization.
EMBED-FN is used to convert the item to a vector."
  (sem-add-batch db sem-default-table-name
                 (list item) (lambda (items)
                               (apply #'vector (mapcar embed-fn items)))
                 write-fn))

(defun sem-build-index (db)
  "Build an index for faster retrieval via ANN on DB."
  (sem-core-build-index db sem-default-table-name))

(defun sem-optimize (db)
  "Run performance optimization routines on the DB.

This includes indexing un-indexed data, as needed."
  (sem-core-optimize db sem-default-table-name))

(defun sem-items-count (db)
  "Return total number of items in the DB."
  (sem-core-items-count db sem-default-table-name))

(defun sem-delete-all (db)
  "Delete all data from the DB."
  (sem-core-delete-all db sem-default-table-name))

(defun sem-similar (db item k embed-fn &optional read-fn)
  "Return K items similar to ITEM from DB.

EMBED-FN is used to convert item to a vector.  READ-FN defaults to
`read' and is used to recover the emacs-lisp object back from the db."
  (let ((results (sem-core-similar db sem-default-table-name (funcall embed-fn item) k)))
    (mapcar (lambda (it) (cons (car it) (funcall (or read-fn #'read) (cdr it)))) results)))

(provide 'sem)

;;; sem.el ends here
