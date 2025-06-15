;;; vecdb-integration-test.el --- Integration tests for vecdb-chroma -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Google LLC
;;
;; Author: Google LLC
;; Maintainer: Google LLC
;; Created: November 2023
;; Modified: November 2023
;; Version: 0.1
;; Keywords: tools, ai
;; Package-Requires: ((emacs "29.1") (vecdb "0.1") (chroma-ext "0.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;;; Commentary:
;;
;;  This file contains integration tests for the `vecdb' package,
;;  supporting multiple backends like Chroma and Qdrant.
;;  Tests for specific backends run if corresponding environment variables are set.
;;
;;  To run these tests manually:
;;  1. Ensure the relevant vector database (Chroma and/or Qdrant) is running.
;;  2. Set the necessary environment variables:
;;     For Chroma: CHROMA_URL (e.g., "http://localhost:8000")
;;                 CHROMA_TENANT (optional, defaults to "default")
;;                 CHROMA_DATABASE (optional, defaults to "default")
;;     For Qdrant: QDRANT_URL (e.g., "http://localhost:6333")
;;                 QDRANT_API_KEY (e.g., "your-api-key")
;;  3. Execute from the command line:
;;     emacs -batch -l ert -l vecdb-integration-test.el -f ert-run-tests-batch-and-exit
;;
;;  The tests for a specific provider will be skipped if its required
;;  environment variables are not set. If no providers are configured,
;;  all tests will be skipped.
;;
;;; Code:

(require 'ert)
(require 'vecdb)
(require 'vecdb-chroma)
(require 'vecdb-qdrant)
(require 'cl-lib) ;; For cl-remove-if-not, cl-every

(declare-function chroma-ext--tmp-project-dir "ext-chroma")

;;; Helper Functions

(defun vecdb-test--get-providers ()
  "Initialize and return a list of available `vecdb-provider' structs.
Reads configuration from environment variables.
Skips tests if no providers are configured."
  (let ((providers (list)))
    ;; ChromaDB Configuration
    (let ((chroma-url (getenv "CHROMA_URL")))
      (when chroma-url
        (add-to-list 'providers
                     (make-vecdb-chroma-provider
                      :url chroma-url
                      :tenant (or (getenv "CHROMA_TENANT") "default")
                      :database (or (getenv "CHROMA_DATABASE") "default"))
                     t)))

    ;; Qdrant Configuration
    (let ((qdrant-url (getenv "QDRANT_URL")))
      (when qdrant-url
        (let ((qdrant-api-key (getenv "QDRANT_API_KEY")))
          (if qdrant-api-key
              (add-to-list 'providers
                           (make-vecdb-qdrant-provider
                            :url qdrant-url
                            :api-key qdrant-api-key)
                           t)
            (warn "QDRANT_URL is set, but QDRANT_API_KEY is missing. Qdrant provider will not be configured.")))))

    (when (null providers)
      (ert-skip "No vector database provider environment variables set. (CHROMA_URL or QDRANT_URL must be set)"))
    providers))

(defmacro vecdb-test--deftest-for-providers (base-name body-function &optional docstring)
  "Define `ert-deftest` forms for BASE-NAME against Chroma and Qdrant providers.
BODY-FUNCTION is a symbol of a function that takes one argument (the provider).
Docstring is optional, a default will be generated.

Each generated test will individually skip if its specific provider
is not found in the list returned by `vecdb-test--get-providers` (which
itself might globally skip if no providers at all are configured)."
  (let ((chroma-test-name (intern (format "%s-chroma" base-name)))
        (qdrant-test-name (intern (format "%s-qdrant" base-name)))
        (base-doc (or docstring (format "Test %s for a vector database provider." base-name))))
    `(progn
       (ert-deftest ,chroma-test-name ()
         ,(format "%s (Chroma)" base-doc)
         (interactive) ; Keep interactive for manual runs if desired
         (let ((current-provider (cl-find-if (lambda (p) (eq (type-of p) 'vecdb-chroma-provider))
                                             (vecdb-test--get-providers))))
           (if current-provider
               (funcall ,body-function current-provider)
             (ert-skip (format "Chroma provider not configured for %s" ',chroma-test-name)))))

       (ert-deftest ,qdrant-test-name ()
         ,(format "%s (Qdrant)" base-doc)
         (interactive)
         (let ((current-provider (cl-find-if (lambda (p) (eq (type-of p) 'vecdb-qdrant-provider))
                                             (vecdb-test--get-providers))))
           (if current-provider
               (funcall ,body-function current-provider)
             (ert-skip (format "Qdrant provider not configured for %s" ',qdrant-test-name))))))))

(defun vecdb-test--make-collection (name vector-size)
  "Return a `vecdb-collection' struct with NAME and VECTOR-SIZE."
  (make-vecdb-collection :name name :vector-size vector-size))

(defun vecdb-test--make-item (id vector payload)
  "Return a `vecdb-item' struct with ID, VECTOR, and PAYLOAD (plist)."
  (make-vecdb-item :id id :vector vector :payload payload))

(defun vecdb-test--random-vector (dimension)
  "Return a vector of DIMENSION with random float numbers."
  (let ((vec (make-vector dimension 0.0)))
    (dotimes (i dimension)
      (setf (aref vec i) (random 1.0)))
    vec))

(defmacro with-test-collection (current-provider collection-var collection-name-base options &rest body)
  "Execute BODY with COLLECTION-VAR bound to a new collection.
CURRENT-PROVIDER is the provider instance.
COLLECTION-VAR is the symbol to bind the collection struct to.
COLLECTION-NAME-BASE is the base string for the collection name.
OPTIONS is a plist, e.g., '(:vector-size 3).
The full collection name is generated by appending the provider's name.
The collection is created before BODY and deleted afterwards."
  (let ((full-collection-name (gensym "full-collection-name-"))
        (vector-size-val (gensym "vector-size-"))
        (default-vector-size 3))
    `(let* ((,full-collection-name (format "%s-%s" ,collection-name-base (vecdb-provider-name ,current-provider)))
            (,vector-size-val (or (plist-get ,options :vector-size) ,default-vector-size))
            (,collection-var (vecdb-test--make-collection ,full-collection-name ,vector-size-val)))
       (unwind-protect
           (progn
             (vecdb-create ,current-provider ,collection-var)
             ,@body)
         ;; Ensure cleanup even if vecdb-create fails or body errors
         (if (vecdb-exists ,current-provider ,collection-var)
             (vecdb-delete ,current-provider ,collection-var)
           (message "Collection %s did not exist or was already deleted during cleanup." ,full-collection-name))))))

;;; Test Suite Definition
;; This will need to be updated with the generated test names.
;; For now, removing the explicit list and :require, relying on ERT to find tests.
;; Or, it could be removed if tests are not run via suite selection.
(def-ert-suite vecdb-integration-suite
  "Integration tests for the `vecdb' package against various backends.")

;;; Test cases Body Functions

(defun vecdb-test-create-exists-delete-collection-body (current-provider)
  "Core logic for testing create, exists, and delete collection."
  (let* ((collection-name-base "test-collection-ced")
         (collection-name (format "%s-%s" collection-name-base (vecdb-provider-name current-provider)))
         (collection (vecdb-test--make-collection collection-name 3)))
    (unwind-protect
        (progn
          (should (vecdb-create current-provider collection))
          (should (vecdb-exists current-provider collection))
          (should (vecdb-delete current-provider collection))
          (should-not (vecdb-exists current-provider collection)))
      ;; Cleanup in case of error during assertions
      (when (vecdb-exists current-provider collection)
        (vecdb-delete current-provider collection)))))

(vecdb-test--deftest-for-providers
 vecdb-test-create-exists-delete-collection
 #'vecdb-test-create-exists-delete-collection-body
 "Test `vecdb-create', `vecdb-exists', and `vecdb-delete'.")

(defun vecdb-test-upsert-get-items-body (current-provider)
  "Core logic for testing upsert and get items."
  (let* ((collection-name "test-collection-ug")
         (vector-size 3)
         (items (list
                 (vecdb-test--make-item "item1" (vecdb-test--random-vector vector-size) '(:val 1))
                 (vecdb-test--make-item "item2" (vecdb-test--random-vector vector-size) '(:val 2))
                 (vecdb-test--make-item "item3" (vecdb-test--random-vector vector-size) '(:val 3)))))
    (with-test-collection current-provider current-collection collection-name `(:vector-size ,vector-size)
      (should (vecdb-upsert-items current-provider current-collection items))
      (dolist (item items)
        (let ((retrieved-item (vecdb-get-item current-provider current-collection (vecdb-item-id item))))
          (should retrieved-item)
          (should (equal (vecdb-item-id item) (vecdb-item-id retrieved-item)))
          (should (equal (vecdb-item-vector item) (vecdb-item-vector retrieved-item)))
          (should (equal (vecdb-item-payload item) (vecdb-item-payload retrieved-item))))))))

(vecdb-test--deftest-for-providers
 vecdb-test-upsert-get-items
 #'vecdb-test-upsert-get-items-body
 "Test `vecdb-upsert-items' and `vecdb-get-item'.")

(defun vecdb-test-delete-items-body (current-provider)
  "Core logic for testing delete items."
  (let* ((collection-name "test-collection-di")
         (vector-size 3)
         (item1 (vecdb-test--make-item "item1" (vecdb-test--random-vector vector-size) '(:val 1)))
         (item2 (vecdb-test--make-item "item2" (vecdb-test--random-vector vector-size) '(:val 2)))
         (item3 (vecdb-test--make-item "item3" (vecdb-test--random-vector vector-size) '(:val 3)))
         (items (list item1 item2 item3)))
    (with-test-collection current-provider current-collection collection-name `(:vector-size ,vector-size)
      (vecdb-upsert-items current-provider current-collection items)
      (should (vecdb-delete-items current-provider current-collection (list (vecdb-item-id item1) (vecdb-item-id item3))))
      (should-not (vecdb-get-item current-provider current-collection (vecdb-item-id item1)))
      (should (vecdb-get-item current-provider current-collection (vecdb-item-id item2)))
      (should-not (vecdb-get-item current-provider current-collection (vecdb-item-id item3))))))

(vecdb-test--deftest-for-providers
 vecdb-test-delete-items
 #'vecdb-test-delete-items-body
 "Test `vecdb-delete-items'.")

(defun vecdb-test-search-by-vector-body (current-provider)
  "Core logic for testing search by vector."
  (let* ((collection-name "test-collection-sv")
         (vector-size 3)
         (item1 (vecdb-test--make-item "item1" (vector 0.1 0.2 0.3) '(:val 1)))
         (item2 (vecdb-test--make-item "item2" (vector 0.4 0.5 0.6) '(:val 2)))
         (item3 (vecdb-test--make-item "item3" (vector 0.7 0.8 0.9) '(:val 3)))
         (items (list item1 item2 item3)))
    (with-test-collection current-provider current-collection collection-name `(:vector-size ,vector-size)
      (vecdb-upsert-items current-provider current-collection items)
      ;; Search for a vector similar to item2
      (let* ((search-vector (vector 0.41 0.51 0.61))
             (results (vecdb-search-by-vector current-provider current-collection search-vector 3)))
        (should (= (length results) 3))
        ;; Item2 should be the closest
        (let ((first-result (car results)))
          (should (equal (vecdb-item-id first-result) (vecdb-item-id item2)))
          (should (equal (vecdb-item-vector first-result) (vecdb-item-vector item2)))
          (should (equal (vecdb-item-payload first-result) (vecdb-item-payload item2))))
        ;; Check if all original items are present in results (order might vary beyond the first)
        (should (cl-every (lambda (item)
                            (cl-find-if (lambda (res-item) (equal (vecdb-item-id item) (vecdb-item-id res-item)))
                                        results))
                          items))))))

(vecdb-test--deftest-for-providers
 vecdb-test-search-by-vector
 #'vecdb-test-search-by-vector-body
 "Test `vecdb-search-by-vector'.")

(defun vecdb-test-search-by-vector-with-filter-body (current-provider)
  "Core logic for testing search by vector with filter."
  (let* ((collection-name "test-collection-svf")
         (vector-size 2)
         (item1 (vecdb-test--make-item "itemA1" (vector 0.1 0.2) '(:type "A" :val 1)))
         (item2 (vecdb-test--make-item "itemB1" (vector 0.3 0.4) '(:type "B" :val 2)))
         (item3 (vecdb-test--make-item "itemA2" (vector 0.5 0.6) '(:type "A" :val 3)))
         (items (list item1 item2 item3)))
    (with-test-collection current-provider current-collection collection-name `(:vector-size ,vector-size)
      (vecdb-upsert-items current-provider current-collection items)
      (let* ((search-vector (vector 0.11 0.21)) ; Closer to itemA1
             ;; Filter for items of type "A"
             (results (vecdb-search-by-vector current-provider current-collection search-vector 3 '(:type "A"))))
        (should (= (length results) 2))
        (should (cl-every (lambda (item) (equal (getf (vecdb-item-payload item) :type) "A")) results))
        ;; itemA1 should be the first result
        (let ((first-result (car results)))
          (should (equal (vecdb-item-id first-result) (vecdb-item-id item1))))
        ;; Ensure itemB1 is not in the results
        (should-not (cl-find-if (lambda (res-item) (equal (vecdb-item-id res-item) "itemB1")) results))))))

(vecdb-test--deftest-for-providers
 vecdb-test-search-by-vector-with-filter
 #'vecdb-test-search-by-vector-with-filter-body
 "Test `vecdb-search-by-vector' with a metadata filter.")


(provide 'vecdb-integration-test)

;;; vecdb-integration-test.el ends here
