;;; dynamic-ring.el --- A dynamically sized ring structure. -*- lexical-binding: t -*-

;; Copyright (C) 2009 Mike Mattie
;; Author: Mike Mattie codermattie@gmail.com
;; Maintainer: Mike Mattie codermattie@gmail.com
;; Created: 2009-4-16
;; Version: 0.0.2

;; This file is NOT a part of Gnu Emacs.

;; License: GPL-v3

;; dynamic-ring.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defconst dynamic-ring-version "0.1.1" "dynamic-ring version")

(eval-when-compile
  (require 'cl))

;;
;; ring structure
;;

(defun make-dyn-ring ()
  "make-dyn-ring

   Return a new dynamic ring stucture. A ring structure is a cons
   cell where the car is the current head element of
   the ring, and the cdr is the number of elements in the ring.
  "
  (cons nil 0))

(defun dyn-ring-head (ring)
  "dyn-ring-head RING

   Return the head segment of the RING.
  "
  (car ring))

(defun dyn-ring-set-head (ring new-head)
  "dyn-ring-set-head RING NEW-HEAD

   Set the head of the RING to NEW-HEAD.
  "
  (setcar ring new-head))

(defun dyn-ring-empty-p (ring)
  "dyn-ring-empty-p RING

   return t if RING has no elements.
  "
  (not (dyn-ring-head ring)))

(defun dyn-ring-size (ring)
  "dyn-ring-size RING

   Return the number of elements in RING.
  "
  (cdr ring))

(defun dyn-ring-set-size (ring new-size)
  "dyn-ring-set-size RING NEW-SIZE

   Set the size of RING to NEW-SIZE.
  "
  (setcdr ring new-size))

(defun dyn-ring-value (ring)
  "dyn-ring-value RING

   Return the value of RING's head segment.
  "
  (let ((head (dyn-ring-head ring)))
    (when head
      (dyn-ring-segment-value head))))

(defun dyn-ring-equal-p (r1 r2)
  "Check if two rings are equal in their values, structure,
and orientation."
  (equal (dyn-ring-values r1)
         (dyn-ring-values r2)))

;;
;; ring segments
;;

(defconst dyn-ring-linkage 0)
(defconst dyn-ring-value   1)

(defun dyn-ring-make-segment (value)
  "dyn-ring-make-segment VALUE

   Create a new dynamic ring segment with VALUE.

   A segment stores a value within a ring with linkage to the
   other segments in the ring. It is an array.

   [linkage,value]

   linkage is a cons cell. The car points to the left segment in
   the ring. The cdr points to the right segment in the ring.
  "
  (let
    ((new-elm (make-vector 2 nil)))
    (aset new-elm dyn-ring-value value)
    (aset new-elm dyn-ring-linkage (cons nil nil))
    new-elm))

(defun dyn-ring-segment-value (segment)
  "dyn-ring-segment-value SEGMENT

   Return the value of SEGMENT.
   "
  (aref segment dyn-ring-value))

(defun dyn-ring-segment-set-value (segment value)
  "dyn-ring-segment-set-value SEGMENT VALUE

   Set the value of SEGMENT to VALUE.
  "
  (aset segment dyn-ring-value value))

(defun dyn-ring-segment-linkage (segment)
  "dyn-ring-segment-linkage SEGMENT

   Return the linkage of SEGMENT.
   "
  (aref segment dyn-ring-linkage))

(defun dyn-ring-segment-previous (segment)
  "dyn-ring-segment-previous SEGMENT

   Return the previous segment in the ring.
   "
  (car (dyn-ring-segment-linkage segment)))

(defun dyn-ring-segment-set-previous (segment new-segment)
  "dyn-ring-segment-set-previous SEGMENT

   Set the previous segment in the ring to NEW-SEGMENT.
   "
  (setcar (dyn-ring-segment-linkage segment) new-segment))

(defun dyn-ring-segment-next (segment)
  "dyn-ring-segment-next SEGMENT

   Return the next segment in the ring.
   "
  (cdr (dyn-ring-segment-linkage segment)))

(defun dyn-ring-segment-set-next (segment new-segment)
  "dyn-ring-segment-set-next SEGMENT

   Set the previous segment in the ring to NEW-SEGMENT.
   "
  (setcdr (dyn-ring-segment-linkage segment) new-segment))

;;
;; ring traversal.
;;

(defun dyn-ring-traverse (ring fn)
  "dyn-ring-traverse RING FN

   walk all of the elements in RING passing each
   element to FN. This performs FN as a side effect and
   does not modify the ring in any way, nor does it
   return a result.
  "
  (let ((head (dyn-ring-head ring)))
    (when head
      (funcall fn (dyn-ring-segment-value head))
      (let ((current (dyn-ring-segment-next head)))
        ;; loop until we return to the head
        (while (and current (not (eq current head)))
          (funcall fn (dyn-ring-segment-value current))
          (setq current (dyn-ring-segment-next current)))
        t))))

(defun dyn-ring-traverse-collect (ring fn)
  "dyn-ring-traverse-collect RING FN

   Walk the elements of RING passing each element to FN.  The
   values of FN for each element is collected into a list and
   returned.
  "
  (let ((output nil))
    (dyn-ring-traverse ring
                       (lambda (element)
                         (push (funcall fn element) output)))
    output))

(defun dyn-ring-map (ring fn)
  "dyn-ring-map RING FN

   Walk the elements of RING passing each element to FN, creating
   a new ring containing the transformed elements. This does not
   modify the original RING.

   dyn-ring-transform-map is a mutating version of this interface.
  "
  (let ((new-ring (make-dyn-ring)))
    (if (dyn-ring-empty-p ring)
        new-ring
      (let ((head (dyn-ring-head ring)))
        (let ((new-head (dyn-ring-insert new-ring
                                         (funcall fn (dyn-ring-segment-value head)))))
          (let ((current (dyn-ring-segment-previous head)))
            (while (not (eq current head))
              (dyn-ring-insert new-ring
                               (funcall fn (dyn-ring-segment-value current)))
              (setq current (dyn-ring-segment-previous current))))
          (dyn-ring-set-head new-ring new-head)
          new-ring)))))

(defun dyn-ring-filter (ring predicate)
  "dyn-ring-filter RING PREDICATE

   Walk the elements of RING passing each element to PREDICATE,
   creating a new ring containing those elements for which
   PREDICATE returns a non-nil result. This does not modify the
   original RING.

   dyn-ring-transform-filter is a mutating version of this interface.
  "
  (let ((new-ring (make-dyn-ring)))
    (if (dyn-ring-empty-p ring)
        new-ring
      (let* ((head (dyn-ring-head ring))
             (current (dyn-ring-segment-previous head))
             (current-value (dyn-ring-segment-value current)))
        (while (not (eq current head))
          ;; go the other way around the ring so that the head
          ;; is the last segment encountered, to avoid having to
          ;; keep track of a potentially changing head
          (when (funcall predicate current-value)
            (dyn-ring-insert new-ring current-value))
          (setq current (dyn-ring-segment-previous current))
          (setq current-value (dyn-ring-segment-value current)))
        ;; check the head
        (when (funcall predicate current-value)
          (let ((new-head (dyn-ring-insert new-ring current-value)))
            (dyn-ring-set-head new-ring new-head)))
        new-ring))))

(defun dyn-ring-transform-filter (ring predicate)
  "dyn-ring-filter RING PREDICATE

   Transform RING by passing each element to PREDICATE,
   retaining only those elements for which
   PREDICATE returns a non-nil result. This mutates the
   existing RING.

   dyn-ring-filter is a functional (non-mutating) version of this
   interface.
  "
  (unless (dyn-ring-empty-p ring)
    (let* ((head (dyn-ring-head ring))
           (current (dyn-ring-segment-previous head))
           (current-value (dyn-ring-segment-value current)))
      (while (not (eq head current))
        ;; go the other way around the ring so that the head
        ;; is the last segment encountered, to avoid having to
        ;; keep track of a potentially changing head
        (let ((previous (dyn-ring-segment-previous current)))
          (unless (funcall predicate current-value)
            (dyn-ring-delete ring current))
          (setq current previous)
          (setq current-value (dyn-ring-segment-value current))))
      ;; check the head
      (unless (funcall predicate current-value)
        (dyn-ring-delete ring current))
      t)))

(defun dyn-ring-transform-map (ring fn)
  "dyn-ring-map RING FN

   Transform the RING by mapping each of its elements under FN.
   This mutates the existing ring.

   dyn-ring-map is a functional (non-mutating) version of this
   interface.
  "
  (unless (dyn-ring-empty-p ring)
    (let ((head (dyn-ring-head ring)))
      (dyn-ring-segment-set-value head
                                  (funcall fn (dyn-ring-segment-value head)))
      (let ((current (dyn-ring-segment-previous head)))
        (while (not (eq current head))
          (dyn-ring-segment-set-value current
                                      (funcall fn (dyn-ring-segment-value current)))
          (setq current (dyn-ring-segment-previous current))))
      (dyn-ring-set-head ring head)
      t)))


;; TODO:
;; (1) map should return a new ring with the elements mapped
;; (2) there should also be a mutating version that modifies the
;;     existing ring under the map (maybe \"transform\")
;; (3) and a function dyn-ring-values that returns a flat list of
;;     values containined in the ring
;; - dyn-ring-map
;; - dyn-ring-filter
;; - dyn-ring-transform-map
;; - dyn-ring-transform-filter
;; - dyn-ring-values

(defun dyn-ring-rotate-until (ring direction fn)
  "dyn-ring-rotate-until RING DIRECTION FN

   Rotate the head of RING in DIRECTION which is one of two
   functions: dyn-ring-rotate-right or dyn-ring-rotate-left.

   The rotation continues until FN predicate which evaluates the
   new head element of each rotation returns non-nil.

   If the predicate does not return non-nil the ring is reset to
   the head element it started with.
  "
  (let
    ((start (dyn-ring-head ring)))

    (catch 'stop

      (when start
        (when (= (dyn-ring-size ring) 1)
          (if (funcall fn (dyn-ring-value ring))
              (throw 'stop t)
            (throw 'stop nil)))

        (funcall direction ring)

        ;; when we have moved off the start loop until we return to it.
        (while (not (eq (dyn-ring-head ring) start))
          (when (funcall fn (dyn-ring-value ring))
            (throw 'stop t))
          (funcall direction ring))
        nil)) ))

(defun dyn-ring-find (ring predicate)
  "dyn-ring-find RING PREDICATE

   Search RING for elements matching PREDICATE, a function that
   evaluates non-nil for for the desired elements.

   The list of matching elements is returned.
  "
  (let ((found nil))
    (dyn-ring-traverse ring
                       (lambda (element)
                         (when (funcall predicate element)
                           (push element found))))
    found))

;;
;; ring modification functions.
;;

(defun dyn-ring-destroy (ring)
  "dyn-ring-destroy  RING

   - INTERNAL -

   Delete the RING. The circular linkage of a ring structure
   makes it doubtful that the garbage collector will be able to
   free a ring without calling dyn-ring-destroy.
  "
  (unless (dyn-ring-empty-p ring)
    (let
        ((current (dyn-ring-head ring)))

      ;; Break the ring by terminating the previous element
      (dyn-ring--free-segment (dyn-ring-segment-previous current))

      (while (dyn-ring-segment-next current)
        (let
            ((next (dyn-ring-segment-next current)))

          ;; delete all the links in the current element
          (dyn-ring--free-segment current)

          ;; move to the right
          (setq current next)))
      ;; delete the head pointer.
      (dyn-ring-set-head ring nil)
      (dyn-ring-set-size ring 0)
      t)))

(defun dyn-ring--link (previous next)
  "Link PREVIOUS and NEXT to one another."
  (dyn-ring-segment-set-previous next previous)
  (dyn-ring-segment-set-next previous next))

(defun dyn-ring-insert (ring element)
  "dyn-ring-insert RING ELEMENT

   Insert ELEMENT into RING. The head of the ring
   will be the new ELEMENT
  "
  (let ((segment (dyn-ring-make-segment element))
        (ring-size (dyn-ring-size ring))
        (head (dyn-ring-head ring)))
    (cond
     ((equal 0 ring-size)
      (dyn-ring--link segment segment))

     (t
      (let ((previous (dyn-ring-segment-previous head)))
        (dyn-ring--link previous segment)
        (dyn-ring--link segment head))))

    ;; point the head at the new segment
    (dyn-ring-set-head ring segment)
    ;; update the element count.
    (dyn-ring-set-size ring (1+ ring-size))

    ;; return the newly inserted segment.
    segment))

(defun dyn-ring--unlink-segment (segment)
  "dyn-ring--unlink-segment SEGMENT

   Unlink SEGMENT by relinking its left and right segments to
   each other.
  "
  (dyn-ring--link (dyn-ring-segment-previous segment)
                  (dyn-ring-segment-next segment)))

(defun dyn-ring--free-segment (segment)
  "dyn-ring--free-segment SEGMENT

   Nullify links in SEGMENT. This is an extra precaution
to make sure that the garbage collector reclaims it (e.g.
if the segment happens to point to itself).
  "
  (dyn-ring-segment-set-next segment nil)
  (dyn-ring-segment-set-previous segment nil))

(defun dyn-ring-delete (ring segment)
  "dyn-ring-delete RING SEGMENT

   Delete SEGMENT from RING.
  "
  (let
    ((ring-size (dyn-ring-size ring)))

    (when (> ring-size 0)
      (cond
       ((equal 1 ring-size)
        (dyn-ring--free-segment (dyn-ring-head ring))
        (dyn-ring-set-head ring nil))
       (t
        (dyn-ring--unlink-segment segment)

        ;; if we deleted the head element set the
        ;; head to the right element.
        (when (eq (dyn-ring-head ring) segment)
          (dyn-ring-set-head ring (dyn-ring-segment-next segment)))
        (dyn-ring--free-segment segment)))
      (dyn-ring-set-size ring (1- (dyn-ring-size ring)))
      t)))

(defun dyn-ring-rotate-left (ring)
  "dyn-ring-rotate-left RING

   Rotate the head of ring to the element left of the current
   head.
  "
  (unless (dyn-ring-empty-p ring)
    (dyn-ring-set-head ring
                       (dyn-ring-segment-previous
                        (dyn-ring-head ring)))))

(defun dyn-ring-rotate-right (ring)
  "dyn-ring-rotate-right RING

   Rotate the head of ring to the element right of the current
   head.
  "
  (unless (dyn-ring-empty-p ring)
    (dyn-ring-set-head ring
                       (dyn-ring-segment-next
                        (dyn-ring-head ring)))))

(defun dyn-ring-values (ring)
  "dyn-ring-values RING

   A list of all values contained in the RING.
  "
  (let ((result nil))
    (dyn-ring-traverse-collect ring
                               (lambda (element)
                                 (push element result)))
    result))

(provide 'dynamic-ring)
;;; dynamic-ring.el ends here
