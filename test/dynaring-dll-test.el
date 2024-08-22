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

(require 'dynaring-dll)
(require 'cl-lib)

;;
;; Fixtures
;;

;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html
(defun fixture-0-dll (body)
  (let ((dll nil))
    (unwind-protect
        (progn (setq dll (dynaring-dll-make))
               (funcall body)))))

;; 1
(defun fixture-1-dll (body)
  (let ((dll nil))
    (unwind-protect
        (progn
          (setq dll (dynaring-dll-make))
          (let ((segment (dynaring-dll-insert-tail dll 1)))
            (funcall body))))))

;; 1 - 2
(defun fixture-2-dll (body)
  (let ((dll nil))
    (unwind-protect
        (progn
          (setq dll (dynaring-dll-make))
          (let ((seg1 (dynaring-dll-insert-tail dll 1))
                (seg2 (dynaring-dll-insert-tail dll 2)))
            (funcall body))))))

;; 1 - 2 - 3
(defun fixture-3-dll (body)
  (let ((dll nil))
    (unwind-protect
        (progn
          (setq dll (dynaring-dll-make))
          (let ((seg1 (dynaring-dll-insert-tail dll 1))
                (seg2 (dynaring-dll-insert-tail dll 2))
                (seg3 (dynaring-dll-insert-tail dll 3)))
            (funcall body))))))

(defun fixture-memo-fn (body)
  (cl-letf ((memofn (lambda (arg)
                      (push arg memo))))
    (unwind-protect
        (funcall body))))

(defmacro with-fixture (fixture &rest test)
  "Run TEST using FIXTURE."
  (declare (indent 1))
  `(,fixture
    (lambda ()
      ,@test)))

;;
;; Test utilities
;;

(defun segments-are-linked-p (previous next)
  (and (eq (dynaring-segment-next previous) next)
       (eq (dynaring-segment-previous next) previous)))

(defun segment-is-free-p (segment)
  (and (null (dynaring-segment-next segment))
       (null (dynaring-segment-previous segment))))

;;
;; Tests
;;

(ert-deftest dynaring-dll-test ()
  ;; null constructor
  (should (dynaring-dll-make))

  ;; variadic constructor
  (should (dynaring-dll 1 2 3))
  (let ((dll (dynaring-dll 1 2 3)))
    (should (dynaring-dll-p dll))
    ;; see the comment on the "variadic insertion"
    ;; test re: ensuring order of insertion
    (should (= 1 (dynaring-segment-value
                  (dynaring-dll-head dll))))
    (should (= 3 (dynaring-segment-value
                  (dynaring-dll-tail dll))))))

(ert-deftest dynaring-dll-p-test ()
  (fixture-0-dll
   (lambda ()
     (should (dynaring-dll-p dll))))
  ;; (should-not (dynaring-dll-p (list 1 2 3)))
  ;; (should-not (dynaring-dll-p (cons 1 "hi")))
  (fixture-1-dll
   (lambda ()
     (should (dynaring-dll-p dll)))))

(ert-deftest dynaring-dll-empty-p-test ()
  (fixture-0-dll
   (lambda ()
     (should (dynaring-dll-empty-p dll))))
  (fixture-1-dll
   (lambda ()
     (should-not (dynaring-dll-empty-p dll)))))

(ert-deftest dynaring-dll-size-test ()
  (fixture-0-dll
   (lambda ()
     (should (= 0 (dynaring-dll-size dll)))))
  (fixture-1-dll
   (lambda ()
     (should (= 1 (dynaring-dll-size dll))))))

(ert-deftest dynaring-dll-head-test ()
  (fixture-0-dll
   (lambda ()
     (should (null (dynaring-dll-head dll)))))
  (fixture-1-dll
   (lambda ()
     (should-not (null (dynaring-dll-head dll))))))

(ert-deftest dynaring-dll-tail-test ()
  (fixture-0-dll
   (lambda ()
     (should (null (dynaring-dll-tail dll)))))
  (fixture-1-dll
   (lambda ()
     (should-not (null (dynaring-dll-tail dll))))))

(ert-deftest dynaring-dll-insert-head-test ()
  (fixture-0-dll
   (lambda ()
     (dynaring-dll-insert-head dll 1)
     (should (= 1 (dynaring-segment-value
                   (dynaring-dll-head dll))))
     (should (eq (dynaring-dll-head dll)
                 (dynaring-dll-tail dll)))))
  (fixture-1-dll
   (lambda ()
     (dynaring-dll-insert-head dll 2)
     (should (= 2 (dynaring-segment-value
                   (dynaring-dll-head dll))))
     (should-not (eq (dynaring-dll-head dll)
                     (dynaring-dll-tail dll)))
     (should (= 1 (dynaring-segment-value
                   (dynaring-dll-tail dll))))))
  (fixture-2-dll
   (lambda ()
     (dynaring-dll-insert-head dll 3)
     (should (= 3 (dynaring-segment-value
                   (dynaring-dll-head dll))))
     (should-not (eq (dynaring-dll-head dll)
                     (dynaring-dll-tail dll))))))

(ert-deftest dynaring-dll-insert-tail-test ()
  (fixture-0-dll
   (lambda ()
     (dynaring-dll-insert-tail dll 1)
     (should (= 1 (dynaring-segment-value
                   (dynaring-dll-tail dll))))
     (should (eq (dynaring-dll-head dll)
                 (dynaring-dll-tail dll)))))
  (fixture-1-dll
   (lambda ()
     (dynaring-dll-insert-tail dll 2)
     (should (= 2 (dynaring-segment-value
                   (dynaring-dll-tail dll))))
     (should-not (eq (dynaring-dll-head dll)
                     (dynaring-dll-tail dll)))
     (should (= 1 (dynaring-segment-value
                   (dynaring-dll-head dll))))))
  (fixture-2-dll
   (lambda ()
     (dynaring-dll-insert-tail dll 3)
     (should (= 3 (dynaring-segment-value
                   (dynaring-dll-tail dll))))
     (should-not (eq (dynaring-dll-head dll)
                     (dynaring-dll-tail dll))))))

(ert-deftest dynaring-dll-insert-before-test ()
  (fixture-1-dll
   (lambda ()
     (dynaring-dll-insert-before dll
                                 (dynaring-dll-head dll)
                                 2)
     (should (= 2 (dynaring-segment-value
                   (dynaring-dll-head dll))))
     (should-not (eq (dynaring-dll-head dll)
                     (dynaring-dll-tail dll)))
     (should (= 1 (dynaring-segment-value
                   (dynaring-dll-tail dll))))))
  (fixture-2-dll
   (lambda ()
     (dynaring-dll-insert-before dll
                                 (dynaring-dll-head dll)
                                 3)
     (should (= 3 (dynaring-segment-value
                   (dynaring-dll-head dll))))
     (should-not (eq (dynaring-dll-head dll)
                     (dynaring-dll-tail dll))))))

(ert-deftest dynaring-dll-insert-after-test ()
  (fixture-1-dll
   (lambda ()
     (dynaring-dll-insert-after dll
                                (dynaring-dll-head dll)
                                2)
     (should (= 1 (dynaring-segment-value
                   (dynaring-dll-head dll))))
     (should-not (eq (dynaring-dll-head dll)
                     (dynaring-dll-tail dll)))
     (should (= 2 (dynaring-segment-value
                   (dynaring-dll-tail dll))))))
  (fixture-2-dll
   (lambda ()
     (dynaring-dll-insert-after dll
                                (dynaring-dll-head dll)
                                3)
     (should (= 1 (dynaring-segment-value
                   (dynaring-dll-head dll))))
     (should-not (eq (dynaring-dll-head dll)
                     (dynaring-dll-tail dll))))))

(ert-deftest dynaring-dll-singleton-p-test ()
  (fixture-0-dll
   (lambda ()
     (should-not (dynaring-dll-singleton-p dll))))
  (fixture-1-dll
   (lambda ()
     (should (dynaring-dll-singleton-p dll))))
  (fixture-2-dll
   (lambda ()
     (should-not (dynaring-dll-singleton-p dll)))))

(ert-deftest dynaring-dll-equal-p-test ()
  (fixture-0-dll
   (lambda ()
     (let ((dll2 (dynaring-dll))
           (dll3 (dynaring-dll 1)))
       (should (dynaring-dll-equal-p dll dll2))
       (should-not (dynaring-dll-equal-p dll dll3)))))
  (fixture-1-dll
   (lambda ()
     (let ((dll2 (dynaring-dll 1))
           (dll3 (dynaring-dll))
           (dll4 (dynaring-dll 1 2)))
       (should (dynaring-dll-equal-p dll dll2))
       (should-not (dynaring-dll-equal-p dll dll3))
       (should-not (dynaring-dll-equal-p dll dll4)))))
  (fixture-2-dll
   (lambda ()
     (let ((dll2 (dynaring-dll 1 2))
           (dll3 (dynaring-dll))
           (dll4 (dynaring-dll 2 1)))
       (should (dynaring-dll-equal-p dll dll2))
       (should-not (dynaring-dll-equal-p dll dll3))
       (should-not (dynaring-dll-equal-p dll dll4)))))
  (fixture-3-dll
   (lambda ()
     (let ((dll2 (dynaring-dll 1 2 3))
           (dll3 (dynaring-dll))
           (dll4 (dynaring-dll 1 3 2)))
       (should (dynaring-dll-equal-p dll dll2))
       (should-not (dynaring-dll-equal-p dll dll3))
       (should-not (dynaring-dll-equal-p dll dll4))))))

(ert-deftest dynaring-dll-traverse-forwards-test ()
  ;; empty ring
  (fixture-0-dll
   (lambda ()
     (with-fixture fixture-memo-fn
       (let ((memo (list)))
         (should-not (dynaring-dll-traverse-forwards dll memofn))
         (should (null memo))))))

  ;; one-element dll
  (fixture-1-dll
   (lambda ()
     (with-fixture fixture-memo-fn
       (let ((memo (list)))
         (should (dynaring-dll-traverse-forwards dll memofn))
         (should (equal memo (list 1)))))))

  ;; two-element dll
  (fixture-2-dll
   (lambda ()
     (with-fixture fixture-memo-fn
       (let ((memo (list)))
         (should (dynaring-dll-traverse-forwards dll memofn))
         (should (equal memo
                        ;; consed each time, so order is reversed
                        (list 2 1)))))))

  ;; 3-element dll
  (fixture-3-dll
   (lambda ()
     (with-fixture fixture-memo-fn
       (let ((memo (list)))
         (should (dynaring-dll-traverse-forwards dll memofn))
         (should (equal memo
                        ;; consed each time, so order is reversed
                        (list 3 2 1))))))))

(ert-deftest dynaring-dll-traverse-backwards-test ()
  ;; empty ring
  (fixture-0-dll
   (lambda ()
     (with-fixture fixture-memo-fn
       (let ((memo (list)))
         (should-not (dynaring-dll-traverse-backwards dll memofn))
         (should (null memo))))))

  ;; one-element dll
  (fixture-1-dll
   (lambda ()
     (with-fixture fixture-memo-fn
       (let ((memo (list)))
         (should (dynaring-dll-traverse-backwards dll memofn))
         (should (equal memo (list 1)))))))

  ;; two-element dll
  (fixture-2-dll
   (lambda ()
     (with-fixture fixture-memo-fn
       (let ((memo (list)))
         (should (dynaring-dll-traverse-backwards dll memofn))
         (should (equal memo
                        ;; consed each time, so order is reversed
                        (list 1 2)))))))

  ;; 3-element dll
  (fixture-3-dll
   (lambda ()
     (with-fixture fixture-memo-fn
       (let ((memo (list)))
         (should (dynaring-dll-traverse-backwards dll memofn))
         (should (equal memo
                        ;; consed each time, so order is reversed
                        (list 1 2 3))))))))
;; HERE
;; dynaring-dll-traverse-collect-test
;; dynaring-dll-map-test

