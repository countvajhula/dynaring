;; Note: we want to retain dynamic binding for these tests because the
;; ERT "fixtures" rely on it.

;; To run the tests from within Emacs, you must `eval-buffer` this test
;; buffer first. Then, run tests using `ert-run-tests-interactively`.

;; Add source paths to load path so the tests can find the source files
;; Adapted from:
;; https://github.com/Lindydancer/cmake-font-lock/blob/47687b6ccd0e244691fb5907aaba609e5a42d787/test/cmake-font-lock-test-setup.el#L20-L27
(defvar dynaring-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".."))
  (add-to-list 'load-path
               (concat dynaring-test-setup-directory dir)))

;;

(require 'dynaring-segment)

;;
;; Tests
;;

(ert-deftest dynaring-make-segment-test ()
  ;; constructor
  (should (dynaring-make-segment 1)))

(ert-deftest dynaring-segment-p-test ()
  ;; predicate
  (should (dynaring-segment-p
           (dynaring-make-segment 1))))

(ert-deftest dynaring-segment-value-test ()
  (should (= 1
             (dynaring-segment-value
              (dynaring-make-segment 1)))))

(ert-deftest dynaring-segment-set-value-test ()
  (let ((segment (dynaring-make-segment 1)))
    (dynaring-segment-set-value segment 2)
    (should (= 2
               (dynaring-segment-value segment)))))

(ert-deftest dynaring-segment-previous-test ()
  (let ((segment (dynaring-make-segment 1)))
    (should-not (dynaring-segment-previous segment)))
  (let ((seg1 (dynaring-make-segment 1))
        (seg2 (dynaring-make-segment 2)))
    (dynaring-segment-set-previous seg1 seg2)
    (should (eq (dynaring-segment-previous seg1)
                seg2))))

(ert-deftest dynaring-segment-next-test ()
  (let ((segment (dynaring-make-segment 1)))
    (should-not (dynaring-segment-next segment)))
  (let ((seg1 (dynaring-make-segment 1))
        (seg2 (dynaring-make-segment 2)))
    (dynaring-segment-set-next seg1 seg2)
    (should (eq (dynaring-segment-next seg1)
                seg2))))
