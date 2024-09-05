;;; dynaring-dll.el --- A dynamically sized ring structure -*- lexical-binding: t -*-

;; Author: Mike Mattie <codermattie@gmail.com>
;;         Sid Kasivajhula <sid@countvajhula.com>
;; Maintainer: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/dynaring
;; Created: 2009-4-16
;; Version: 0.3
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT a part of Gnu Emacs.

;; This work is "part of the world."  You are free to do whatever you
;; like with it and it isn't owned by anybody, not even the
;; creators.  Attribution would be appreciated and is a valuable
;; contribution in itself, but it is not strictly necessary nor
;; required.  If you'd like to learn more about this way of doing
;; things and how it could lead to a peaceful, efficient, and creative
;; world, and how you can help, visit https://drym.org.
;;
;; This paradigm transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.

;;; Commentary:

;; A doubly-linked list.

;;; Code:

(require 'dynaring-segment)

;;
;; DLL structure
;;

(defun dynaring-dll-make ()
  "Return a new doubly linked list (DLL).

A DLL is a cons pair where the `car' is a reference to the head
segment and the `cdr' is a reference to the tail."
  (cons nil nil))

(defun dynaring-dll-p (value)
  "Check if VALUE is a DLL.

This is a crude predicate that just checks whether VALUE _could be_
treated as a DLL."
  (consp value))

(defun dynaring-dll-head (dll)
  "Return the head segment of the DLL."
  (car dll))

(defun dynaring-dll-tail (dll)
  "Return the tail segment of the DLL."
  (cdr dll))

(defun dynaring-dll-set-head (dll segment)
  "Set the head of the DLL to SEGMENT."
  (setcar dll segment))

(defun dynaring-dll-set-tail (dll segment)
  "Set the tail of the DLL to SEGMENT."
  (setcdr dll segment))

(defun dynaring-dll-insert-before (dll segment element)
  "Insert ELEMENT into the DLL before SEGMENT."
  (let ((new-segment (dynaring-make-segment element)))
    (dynaring--link new-segment segment)
    (when (eq segment (dynaring-dll-head dll))
      (dynaring-dll-set-head dll new-segment))
    new-segment))

(defun dynaring-dll-insert-after (dll segment element)
  "Insert ELEMENT into the DLL after SEGMENT."
  (let ((new-segment (dynaring-make-segment element)))
    (dynaring--link segment new-segment)
    (when (eq segment (dynaring-dll-tail dll))
      (dynaring-dll-set-tail dll new-segment))
    new-segment))

(defun dynaring-dll-insert-head (dll element)
  "Insert ELEMENT into the DLL as the new head."
  (let ((orig-head (dynaring-dll-head dll)))
    (if orig-head
        (dynaring-dll-insert-before dll orig-head element)
      (let ((segment (dynaring-make-segment element)))
        ;; point head and tail at the new segment
        (dynaring-dll-set-head dll segment)
        (dynaring-dll-set-tail dll segment)
        ;; return the newly inserted segment.
        segment))))

(defun dynaring-dll-insert-tail (dll element)
  "Insert ELEMENT into the DLL as the new tail."
  (let ((orig-tail (dynaring-dll-tail dll)))
    (if orig-tail
        (dynaring-dll-insert-after dll orig-tail element)
      (let ((segment (dynaring-make-segment element)))
        ;; point head and tail at the new segment
        (dynaring-dll-set-head dll segment)
        (dynaring-dll-set-tail dll segment)
        ;; return the newly inserted segment.
        segment))))

(defun dynaring-dll-empty-p (dll)
  "Return t if DLL has no elements."
  (null (dynaring-dll-head dll)))

(defun dynaring-dll-singleton-p (dll)
  "Return t if DLL has exactly one element."
  (and (not (dynaring-dll-empty-p dll))
       (eq (dynaring-dll-head dll)
           (dynaring-dll-tail dll))))

(defun dynaring-dll-size (dll)
  "Return the number of elements in DLL."
  (let ((size 0))
    (dynaring-dll-traverse-forwards dll
                                    (lambda (_v)
                                      (setq size (1+ size))))
    size))

(defun dynaring-dll-equal-p (d1 d2)
  "Check if two DLLs D1 and D2 are equal.

Equality of DLLs is defined in terms of contained values and order."
  (equal (dynaring-dll-values d1)
         (dynaring-dll-values d2)))

;;
;; DLL traversal.
;;

(defun dynaring-dll--traverse (start end fn next-fn)
  "Walk the elements of DLL passing each element to FN.

Traverse the DLL from START to END using NEXT-FN, which is expected to
be either `dynaring-segment-previous' or `dynaring-segment-next'.

This performs FN as a side effect and does not modify the DLL in any
way, nor does it return a result."
  (let ((current start))
    ;; loop until we return to the head
    (while (and current (not (eq current end)))
      (funcall fn (dynaring-segment-value current))
      (setq current (funcall next-fn current)))
    ;; one more time for the tail
    (funcall fn (dynaring-segment-value current))
    t))

(defun dynaring-dll-traverse-forwards (dll fn)
  "Walk the elements of DLL passing each element to FN.

This performs FN as a side effect and does not modify the DLL in any
way, nor does it return a result."
  (let ((head (dynaring-dll-head dll))
        (tail (dynaring-dll-tail dll)))
    (when head
      (dynaring-dll--traverse head
                              tail
                              fn
                              #'dynaring-segment-next))))

(defun dynaring-dll-traverse-backwards (dll fn)
  "Walk the elements of DLL passing each element to FN.

This performs FN as a side effect and does not modify the DLL in any
way, nor does it return a result."
  (let ((head (dynaring-dll-head dll))
        (tail (dynaring-dll-tail dll)))
    (when tail
      (dynaring-dll--traverse tail
                              head
                              fn
                              #'dynaring-segment-previous))))

(defun dynaring-dll-traverse-collect (dll fn)
  "Walk the elements of DLL passing each element to FN.

The values of FN for each element are collected into a list and
returned."
  (let ((output nil))
    (dynaring-dll-traverse-backwards dll
                                     (lambda (element)
                                       (push (funcall fn element) output)))
    output))

(defun dynaring-dll-map (dll fn)
  "Derive a new DLL by transforming DLL under FN.

Walk the elements of DLL passing each element to FN, creating a new
DLL containing the transformed elements.  This does not modify the
original DLL.

`dynaring-dll-transform-map` is a mutating version of this interface."
  (let ((new-dll (dynaring-dll-make)))
    (if (dynaring-dll-empty-p dll)
        new-dll
      (let ((current (dynaring-dll-head dll))
            (tail (dynaring-dll-tail dll)))
        (while (not (eq current tail))
          (dynaring-dll-insert-tail new-dll
                                    (funcall fn
                                             (dynaring-segment-value current)))
          (setq current (dynaring-segment-next current)))
        (dynaring-dll-insert-tail new-dll
                                  (funcall fn
                                           (dynaring-segment-value current)))
        new-dll))))

(defun dynaring-dll-filter (dll predicate)
  "Derive a new DLL by filtering DLL using PREDICATE.

Walk the elements of DLL passing each element to PREDICATE, creating
a new DLL containing those elements for which PREDICATE returns a
non-nil result.  This does not modify the original DLL.

`dynaring-dll-transform-filter` is a mutating version of this
interface."
  (let ((new-dll (dynaring-dll-make)))
    (if (dynaring-dll-empty-p dll)
        new-dll
      (let ((current (dynaring-dll-head dll))
            (tail (dynaring-dll-tail dll)))
        (while (not (eq current tail))
          (let ((value (dynaring-segment-value current)))
            (when (funcall predicate value)
              (dynaring-dll-insert-tail new-dll
                                        value))
            (setq current (dynaring-segment-next current))))
        (let ((value (dynaring-segment-value current)))
          (when (funcall predicate value)
            (dynaring-dll-insert-tail new-dll
                                      value)))
        new-dll))))

(defun dynaring-dll-transform-map (dll fn)
  "Transform the DLL by mapping each of its elements under FN.

This mutates the existing DLL.

`dynaring-dll-map` is a functional (non-mutating) version of this
interface."
  (unless (dynaring-dll-empty-p dll)
    (let ((current (dynaring-dll-head dll))
          (tail (dynaring-dll-tail dll)))
      (while (not (eq current tail))
        (dynaring-segment-set-value current
                                    (funcall fn (dynaring-segment-value current)))
        (setq current (dynaring-segment-next current)))
      (dynaring-segment-set-value current
                                  (funcall fn (dynaring-segment-value current)))
      t)))

(defun dynaring-dll-transform-filter (dll predicate)
  "Transform DLL by filtering its elements using PREDICATE.

This retains only those elements for which PREDICATE returns a non-nil
result.  This mutates the existing DLL.

`dynaring-dll-filter` is a functional (non-mutating) version of this
interface."
  (unless (dynaring-dll-empty-p dll)
    (let ((current (dynaring-dll-head dll))
          (tail (dynaring-dll-tail dll)))
      (while (not (eq tail current))
        (if (funcall predicate (dynaring-segment-value current))
            (setq current (dynaring-segment-next current))
          (let ((next (dynaring-segment-next current)))
            (dynaring-dll-delete-segment dll current)
            (setq current next))))
      (unless (funcall predicate (dynaring-segment-value current))
        (dynaring-dll-delete-segment dll current))
      t)))

(defun dynaring-dll--find (dll predicate start direction)
  "Search DLL for an element matching a PREDICATE.

Searches in DIRECTION starting from segment START for the first
element that matches PREDICATE.  DIRECTION must be either
`dynaring-segment-next` (to search forward) or
`dynaring-segment-previous` (to search backwards).

The segment containing the matching element is returned, or nil
if a matching element isn't found."
  (unless (dynaring-dll-empty-p dll)
    (let ((current start))
      (if (funcall predicate
                   (dynaring-segment-value current))
          current
        (catch 'stop
          (let ((current (funcall direction current)))
            (while current
              (when (funcall predicate
                             (dynaring-segment-value current))
                (throw 'stop current))
              (setq current (funcall direction current)))))))))

(defun dynaring-dll-find-forwards (dll predicate)
  "Search DLL in the forward direction.

Searches for the first element that matches PREDICATE.

The segment containing the matching element is returned, or nil
if a matching element isn't found."
  (dynaring-dll--find dll
                      predicate
                      (dynaring-dll-head dll)
                      #'dynaring-segment-next))

(defun dynaring-dll-find-backwards (dll predicate)
  "Search DLL in the backward direction.

Searches for the first element that matches PREDICATE.

The segment containing the matching element is returned, or nil
if a matching element isn't found."
  (dynaring-dll--find dll
                      predicate
                      (dynaring-dll-tail dll)
                      #'dynaring-segment-previous))

(defun dynaring-dll-contains-p (dll element)
  "Predicate to check whether ELEMENT is in DLL."
  (dynaring-dll-find-forwards dll
                              (lambda (elem)
                                (eq elem element))))

;;
;; DLL modification functions.
;;

(defun dynaring-dll (&rest elements)
  "Create a new doubly-linked list (DLL) containing ELEMENTS.

The elements are inserted in order at the tail, so that the first one
inserted becomes the head, and the last one inserted becomes the tail.
See `dynaring-dll-make' for more on the data structure."
  (let ((dll (dynaring-dll-make)))
    (dolist (element elements)
      (dynaring-dll-insert-tail dll element))
    dll))

(defun dynaring-dll-delete-segment (dll segment)
  "Delete SEGMENT from DLL."
  (unless (dynaring-dll-empty-p dll)
    (cond
     ((dynaring-dll-singleton-p dll)
      (dynaring-dll-set-head dll nil)
      (dynaring-dll-set-tail dll nil))
     (t
      (dynaring--unlink-segment segment)

      ;; reset the head or tail if we are deleting it
      (when (eq (dynaring-dll-head dll) segment)
        (dynaring-dll-set-head dll (dynaring-segment-next segment)))
      (when (eq (dynaring-dll-tail dll) segment)
        (dynaring-dll-set-tail dll (dynaring-segment-previous segment)))))
    (dynaring--free-segment segment)
    t))

(defun dynaring-dll-delete (dll element)
  "Delete ELEMENT from DLL."
  (let ((segment (dynaring-dll-find-forwards dll
                                             (lambda (elem)
                                               (eq elem element)))))
    (when segment
      (dynaring-dll-delete-segment dll segment))))

(defun dynaring-dll-values (dll)
  "A list of all values contained in the DLL.

The values are obtained by traversing the DLL from the head to the
tail."
  (dynaring-dll-traverse-collect dll #'identity))

(provide 'dynaring-dll)
;;; dynaring-dll.el ends here
