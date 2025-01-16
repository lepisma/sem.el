;;; sem-embed.el --- Embedding functions for sem -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Embedding functions for sem
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

(require 'tokenizers)
(require 'onnx)
(require 'onnx-ml-utils)

(defcustom sem-embed-model-path nil
  "Path to the ONNX file for all-MiniLM-L6-v2 model."
  :type 'string)

(defvar sem-embed-dim 384
  "Embedding dimension for the default model.")

(defvar sem-embed-tokenizer nil
  "Placeholder for keeping the default tokenizer.")

(defvar sem-embed-model nil
  "Placeholder for keeping the default model.")

(defun sem-embed-load-tokenizer ()
  "Load the default tokenizer, save in variable, and return."
  (setq sem-embed-tokenizer (tokenizers-from-pretrained "sentence-transformers/all-MiniLM-L6-v2"))
  (tokenizers-enable-padding sem-embed-tokenizer 0 "[PAD]")
  sem-embed-tokenizer)

(defun sem-embed-load-model ()
  "Load the default embedding model, save in variable, and return."
  (if (not sem-embed-model-path)
      (error "`sem-embed-model-path' not set")
    (setq sem-embed-model (onnx-load sem-embed-model-path))
    sem-embed-model))

(defun sem-embed-default (items &optional write-fn)
  "Default embedding function for ITEMS.

This serializes the items via write-fn (defaults to prin1-to-string) and
embeds using the all-MiniLM-L6-v2 model."
  (let* ((string-reps (apply #'vector (mapcar (or write-fn #'prin1-to-string) items)))
         (encodings (tokenizers-encode-batch (or sem-embed-tokenizer (sem-embed-load-tokenizer)) string-reps t))
         (output (onnx-run (or sem-embed-model (sem-embed-load-model)) `(("input_ids" . ,(nth 0 encodings))
                                                                         ("token_type_ids" . ,(nth 1 encodings))
                                                                         ("attention_mask" . ,(nth 2 encodings)))
                           '("last_hidden_state"))))
    (setq output (onnx-ml-utils-nmean-pool output (nth 2 encodings)))
    (onnx-ml-utils-nl2-normalize output)
    output))

(provide 'sem-embed)

;;; sem-embed.el ends here
