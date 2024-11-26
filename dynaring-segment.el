;;; dynaring-segment.el --- A dynamically sized ring structure -*- lexical-binding: t -*-

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

;; A dynamically sized ring structure.

;;; Code:

;;
;; ring segments
;;

(defconst dynaring-linkage 0)
(defconst dynaring-value   1)

(defun dynaring-make-segment (value)
  "Create a new dynamic ring segment containing VALUE.

A segment stores a value within a ring with linkage to the
other segments in the ring.  It is an array.

[linkage,value]

linkage is a cons cell.  The car points to the left segment in
the ring.  The cdr points to the right segment in the ring."
  (let
    ((new-elm (make-vector 2 nil)))
    (aset new-elm dynaring-value value)
    (aset new-elm dynaring-linkage (cons nil nil))
    new-elm))

(defun dynaring-segment-p (value)
  "Check if VALUE is a dynaring segment.

This is a crude predicate that could be improved if needed."
  (vectorp value))

(defun dynaring-segment-value (segment)
  "Return the value of SEGMENT."
  (aref segment dynaring-value))

(defun dynaring-segment-set-value (segment value)
  "Set the value of SEGMENT to VALUE."
  (aset segment dynaring-value value))

(defun dynaring-segment-linkage (segment)
  "Return the linkage of SEGMENT."
  (aref segment dynaring-linkage))

(defun dynaring-segment-previous (segment)
  "Return the previous SEGMENT in the ring."
  (car (dynaring-segment-linkage segment)))

(defun dynaring-segment-set-previous (segment new-segment)
  "Set the previous SEGMENT in the ring to NEW-SEGMENT."
  (setcar (dynaring-segment-linkage segment) new-segment))

(defun dynaring-segment-next (segment)
  "Return the next SEGMENT in the ring."
  (cdr (dynaring-segment-linkage segment)))

(defun dynaring-segment-set-next (segment new-segment)
  "Set the previous SEGMENT in the ring to NEW-SEGMENT."
  (setcdr (dynaring-segment-linkage segment) new-segment))

(defun dynaring--link (previous next)
  "Link PREVIOUS and NEXT to one another."
  (when next
    (dynaring-segment-set-previous next previous))
  (when previous
    (dynaring-segment-set-next previous next)))

(defun dynaring--unlink-segment (segment)
  "Unlink SEGMENT from its neighboring segments.

Unlinks the SEGMENT by relinking its left and right segments to
each other."
  (dynaring--link (dynaring-segment-previous segment)
                  (dynaring-segment-next segment)))

(defun dynaring--free-segment (segment)
  "Nullify links in SEGMENT.

This is an extra precaution to make sure that the garbage collector
reclaims it (e.g. if the segment happens to point to itself)."
  (dynaring-segment-set-next segment nil)
  (dynaring-segment-set-previous segment nil))

(provide 'dynaring-segment)
;;; dynaring-segment.el ends here
