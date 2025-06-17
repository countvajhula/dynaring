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

(defun fixture-memo (body)
  (cl-letf ((memofn (lambda (arg)
                      (push arg memo)))
            (memo (list)))
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
  (with-fixture fixture-0-dll
    (should (dynaring-dll-p dll)))
  ;; (should-not (dynaring-dll-p (list 1 2 3)))
  ;; (should-not (dynaring-dll-p (cons 1 "hi")))
  (with-fixture fixture-1-dll
    (should (dynaring-dll-p dll))))

(ert-deftest dynaring-dll-empty-p-test ()
  (with-fixture fixture-0-dll
    (should (dynaring-dll-empty-p dll)))
  (with-fixture fixture-1-dll
    (should-not (dynaring-dll-empty-p dll))))

(ert-deftest dynaring-dll-size-test ()
  (with-fixture fixture-0-dll
    (should (= 0 (dynaring-dll-size dll))))
  (with-fixture fixture-1-dll
    (should (= 1 (dynaring-dll-size dll)))))

(ert-deftest dynaring-dll-head-test ()
  (with-fixture fixture-0-dll
    (should (null (dynaring-dll-head dll))))
  (with-fixture fixture-1-dll
    (should-not (null (dynaring-dll-head dll)))))

(ert-deftest dynaring-dll-tail-test ()
  (with-fixture fixture-0-dll
    (should (null (dynaring-dll-tail dll))))
  (with-fixture fixture-1-dll
    (should-not (null (dynaring-dll-tail dll)))))

(ert-deftest dynaring-dll-insert-head-test ()
  (with-fixture fixture-0-dll
    (dynaring-dll-insert-head dll 1)
    (should (= 1 (dynaring-segment-value
                  (dynaring-dll-head dll))))
    (should (eq (dynaring-dll-head dll)
                (dynaring-dll-tail dll))))
  (with-fixture fixture-1-dll
    (dynaring-dll-insert-head dll 2)
    (should (= 2 (dynaring-segment-value
                  (dynaring-dll-head dll))))
    (should-not (eq (dynaring-dll-head dll)
                    (dynaring-dll-tail dll)))
    (should (= 1 (dynaring-segment-value
                  (dynaring-dll-tail dll)))))
  (with-fixture fixture-2-dll
    (dynaring-dll-insert-head dll 3)
    (should (= 3 (dynaring-segment-value
                  (dynaring-dll-head dll))))
    (should-not (eq (dynaring-dll-head dll)
                    (dynaring-dll-tail dll)))))

(ert-deftest dynaring-dll-insert-tail-test ()
  (with-fixture fixture-0-dll
    (dynaring-dll-insert-tail dll 1)
    (should (= 1 (dynaring-segment-value
                  (dynaring-dll-tail dll))))
    (should (eq (dynaring-dll-head dll)
                (dynaring-dll-tail dll))))
  (with-fixture fixture-1-dll
    (dynaring-dll-insert-tail dll 2)
    (should (= 2 (dynaring-segment-value
                  (dynaring-dll-tail dll))))
    (should-not (eq (dynaring-dll-head dll)
                    (dynaring-dll-tail dll)))
    (should (= 1 (dynaring-segment-value
                  (dynaring-dll-head dll)))))
  (with-fixture fixture-2-dll
    (dynaring-dll-insert-tail dll 3)
    (should (= 3 (dynaring-segment-value
                  (dynaring-dll-tail dll))))
    (should-not (eq (dynaring-dll-head dll)
                    (dynaring-dll-tail dll)))))

(ert-deftest dynaring-dll-insert-before-test ()
  (with-fixture fixture-0-dll
    (should-error
     (dynaring-dll-insert-before dll nil 1)))
  (with-fixture fixture-1-dll
    (dynaring-dll-insert-before dll
                                (dynaring-dll-head dll)
                                2)
    (should (= 2 (dynaring-segment-value
                  (dynaring-dll-head dll))))
    (should-not (eq (dynaring-dll-head dll)
                    (dynaring-dll-tail dll)))
    (should (= 1 (dynaring-segment-value
                  (dynaring-dll-tail dll)))))
  (with-fixture fixture-2-dll
    (dynaring-dll-insert-before dll
                                (dynaring-dll-head dll)
                                3)
    (should (= 3 (dynaring-segment-value
                  (dynaring-dll-head dll))))
    (should-not (eq (dynaring-dll-head dll)
                    (dynaring-dll-tail dll)))))

(ert-deftest dynaring-dll-insert-after-test ()
  (with-fixture fixture-0-dll
    (should-error
     (dynaring-dll-insert-after dll nil 1)))
  (with-fixture fixture-1-dll
    (dynaring-dll-insert-after dll
                               (dynaring-dll-head dll)
                               2)
    (should (= 1 (dynaring-segment-value
                  (dynaring-dll-head dll))))
    (should-not (eq (dynaring-dll-head dll)
                    (dynaring-dll-tail dll)))
    (should (= 2 (dynaring-segment-value
                  (dynaring-dll-tail dll)))))
  (with-fixture fixture-2-dll
    (dynaring-dll-insert-after dll
                               (dynaring-dll-head dll)
                               3)
    (should (= 1 (dynaring-segment-value
                  (dynaring-dll-head dll))))
    (should-not (eq (dynaring-dll-head dll)
                    (dynaring-dll-tail dll)))))

(ert-deftest dynaring-dll-singleton-p-test ()
  (with-fixture fixture-0-dll
    (should-not (dynaring-dll-singleton-p dll)))
  (with-fixture fixture-1-dll
    (should (dynaring-dll-singleton-p dll)))
  (with-fixture fixture-2-dll
    (should-not (dynaring-dll-singleton-p dll))))

(ert-deftest dynaring-dll-equal-p-test ()
  (with-fixture fixture-0-dll
    (let ((dll2 (dynaring-dll))
          (dll3 (dynaring-dll 1)))
      (should (dynaring-dll-equal-p dll dll2))
      (should-not (dynaring-dll-equal-p dll dll3))))
  (with-fixture fixture-1-dll
    (let ((dll2 (dynaring-dll 1))
          (dll3 (dynaring-dll))
          (dll4 (dynaring-dll 1 2)))
      (should (dynaring-dll-equal-p dll dll2))
      (should-not (dynaring-dll-equal-p dll dll3))
      (should-not (dynaring-dll-equal-p dll dll4))))
  (with-fixture fixture-2-dll
    (let ((dll2 (dynaring-dll 1 2))
          (dll3 (dynaring-dll))
          (dll4 (dynaring-dll 2 1)))
      (should (dynaring-dll-equal-p dll dll2))
      (should-not (dynaring-dll-equal-p dll dll3))
      (should-not (dynaring-dll-equal-p dll dll4))))
  (with-fixture fixture-3-dll
    (let ((dll2 (dynaring-dll 1 2 3))
          (dll3 (dynaring-dll))
          (dll4 (dynaring-dll 1 3 2)))
      (should (dynaring-dll-equal-p dll dll2))
      (should-not (dynaring-dll-equal-p dll dll3))
      (should-not (dynaring-dll-equal-p dll dll4)))))

(ert-deftest dynaring-dll-traverse-forwards-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (with-fixture fixture-memo
      (should-not (dynaring-dll-traverse-forwards dll memofn))
      (should (null memo))))

  ;; one-element dll
  (with-fixture fixture-1-dll
    (with-fixture fixture-memo
      (should (dynaring-dll-traverse-forwards dll memofn))
      (should (equal memo (list 1)))))

  ;; two-element dll
  (with-fixture fixture-2-dll
    (with-fixture fixture-memo
      (should (dynaring-dll-traverse-forwards dll memofn))
      (should (equal memo
                     ;; consed each time, so order is reversed
                     (list 2 1)))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (with-fixture fixture-memo
      (should (dynaring-dll-traverse-forwards dll memofn))
      (should (equal memo
                     ;; consed each time, so order is reversed
                     (list 3 2 1))))))

(ert-deftest dynaring-dll-traverse-backwards-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (with-fixture fixture-memo
      (should-not (dynaring-dll-traverse-backwards dll memofn))
      (should (null memo))))

  ;; one-element dll
  (with-fixture fixture-1-dll
    (with-fixture fixture-memo
      (should (dynaring-dll-traverse-backwards dll memofn))
      (should (equal memo (list 1)))))

  ;; two-element dll
  (with-fixture fixture-2-dll
    (with-fixture fixture-memo
      (should (dynaring-dll-traverse-backwards dll memofn))
      (should (equal memo
                     ;; consed each time, so order is reversed
                     (list 1 2)))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (with-fixture fixture-memo
      (should (dynaring-dll-traverse-backwards dll memofn))
      (should (equal memo
                     ;; consed each time, so order is reversed
                     (list 1 2 3))))))

(ert-deftest dynaring-dll-traverse-collect-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (should-not (dynaring-dll-traverse-collect dll #'1+)))

  ;; one-element dll
  (with-fixture fixture-1-dll
    (should (equal (dynaring-dll-traverse-collect dll #'1+)
                   (list 2))))

  ;; two-element dll
  (with-fixture fixture-2-dll
    (should (equal (dynaring-dll-traverse-collect dll #'1+)
                   (list 2 3))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (should (equal (dynaring-dll-traverse-collect dll #'1+)
                   (list 2 3 4)))))

(ert-deftest dynaring-dll-map-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (let ((result (dynaring-dll-map dll #'1+)))
      (should (dynaring-dll-empty-p result))))

  ;; one-element dll
  (with-fixture fixture-1-dll
    (let ((result (dynaring-dll-map dll #'1+)))
      (should
       (equal (dynaring-dll-values result)
              (seq-map #'1+ (dynaring-dll-values dll))))))

  ;; two-element dll
  (with-fixture fixture-2-dll
    (let ((result (dynaring-dll-map dll #'1+)))
      (should (equal (dynaring-dll-values result)
                     (seq-map #'1+ (dynaring-dll-values dll))))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (let ((result (dynaring-dll-map dll #'1+)))
      (should (equal (dynaring-dll-values result)
                     (seq-map #'1+ (dynaring-dll-values dll)))))))

(ert-deftest dynaring-dll-transform-map-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (dynaring-dll-transform-map dll #'1+)
    (should (dynaring-dll-empty-p dll)))

  ;; one-element dll
  (with-fixture fixture-1-dll
    (dynaring-dll-transform-map dll #'1+)
    (should
     (equal (dynaring-dll-values dll)
            (seq-map #'1+ (list 1)))))

  ;; two-element dll
  (with-fixture fixture-2-dll
    (dynaring-dll-transform-map dll #'1+)
    (should (equal (dynaring-dll-values dll)
                   (seq-map #'1+ (list 1 2)))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (dynaring-dll-transform-map dll #'1+)
    (should (equal (dynaring-dll-values dll)
                   (seq-map #'1+ (list 1 2 3))))))

(ert-deftest dynaring-dll-filter-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (let ((dll2 (dynaring-dll-filter dll #'cl-oddp)))
      (should (dynaring-dll-empty-p dll2))))

  ;; one-element dll
  (with-fixture fixture-1-dll
    (let ((dll2 (dynaring-dll-filter dll #'cl-oddp)))
      (should
       (equal (dynaring-dll-values dll2)
              (seq-filter #'cl-oddp (list 1))))))
  (let ((dll (dynaring-dll 2)))
    (let ((dll2 (dynaring-dll-filter dll #'cl-oddp)))
      (should (dynaring-dll-empty-p dll2))))

  ;; two-element dll
  (with-fixture fixture-2-dll
    (let ((dll2 (dynaring-dll-filter dll #'cl-oddp)))
      (should (equal (dynaring-dll-values dll2)
                     (seq-filter #'cl-oddp (list 1))))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (let ((dll2 (dynaring-dll-filter dll #'cl-oddp)))
      (should (equal (dynaring-dll-values dll2)
                     (seq-filter #'cl-oddp (list 1 3)))))))

(ert-deftest dynaring-dll-transform-filter-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (dynaring-dll-transform-filter dll #'cl-oddp)
    (should (dynaring-dll-empty-p dll)))

  ;; one-element dll
  (with-fixture fixture-1-dll
    (dynaring-dll-transform-filter dll #'cl-oddp)
    (should
     (equal (dynaring-dll-values dll)
            (seq-filter #'cl-oddp (list 1)))))
  (let ((dll (dynaring-dll 2)))
    (dynaring-dll-transform-filter dll #'cl-oddp)
    (should (dynaring-dll-empty-p dll)))

  ;; two-element dll
  (with-fixture fixture-2-dll
    (dynaring-dll-transform-filter dll #'cl-oddp)
    (should (equal (dynaring-dll-values dll)
                   (seq-filter #'cl-oddp (list 1)))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (dynaring-dll-transform-filter dll #'cl-oddp)
    (should (equal (dynaring-dll-values dll)
                   (seq-filter #'cl-oddp (list 1 3))))))

(ert-deftest dynaring-dll-find-forwards-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (should-not
     (dynaring-dll-find-forwards dll
                                 (lambda (element)
                                   t))))

  ;; 1-element dll
  (with-fixture fixture-1-dll
    (should
     (eq segment
         (dynaring-dll-find-forwards dll
                                     (lambda (element)
                                       (= 1 element)))))
    (should-not
     (dynaring-dll-find-forwards dll
                                 (lambda (element)
                                   nil))))

  ;; 2-element dll
  (with-fixture fixture-2-dll
    (should
     (eq seg1
         (dynaring-dll-find-forwards dll
                                     (lambda (element)
                                       (= 1 element)))))
    (should
     (eq seg1 (dynaring-dll-find-forwards dll
                                          (lambda (element)
                                            (< element 3)))))
    (should
     (eq seg2 (dynaring-dll-find-forwards dll
                                          (lambda (element)
                                            (> element 1)))))
    (should-not
     (dynaring-dll-find-forwards dll
                                 (lambda (element)
                                   nil))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (should
     (eq seg1
         (dynaring-dll-find-forwards dll
                                     (lambda (element)
                                       (= 1 element)))))
    (should
     (eq seg1 (dynaring-dll-find-forwards dll
                                          (lambda (element)
                                            (< element 4)))))
    (should
     (eq seg3 (dynaring-dll-find-forwards dll
                                          (lambda (element)
                                            (> element 2)))))
    (should-not
     (dynaring-dll-find-forwards dll
                                 (lambda (element)
                                   nil)))))

(ert-deftest dynaring-dll-find-backwards-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (should-not
     (dynaring-dll-find-backwards dll
                                  (lambda (element)
                                    t))))

  ;; 1-element dll
  (with-fixture fixture-1-dll
    (should
     (eq segment
         (dynaring-dll-find-backwards dll
                                      (lambda (element)
                                        (= 1 element)))))
    (should-not
     (dynaring-dll-find-backwards dll
                                  (lambda (element)
                                    nil))))

  ;; 2-element dll
  (with-fixture fixture-2-dll
    (should
     (eq seg1
         (dynaring-dll-find-backwards dll
                                      (lambda (element)
                                        (= 1 element)))))
    (should
     (eq seg2 (dynaring-dll-find-backwards dll
                                           (lambda (element)
                                             (< element 3)))))
    (should
     (eq seg2 (dynaring-dll-find-backwards dll
                                           (lambda (element)
                                             (> element 1)))))
    (should-not
     (dynaring-dll-find-backwards dll
                                  (lambda (element)
                                    nil))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (should
     (eq seg1
         (dynaring-dll-find-backwards dll
                                      (lambda (element)
                                        (= 1 element)))))
    (should
     (eq seg3 (dynaring-dll-find-backwards dll
                                           (lambda (element)
                                             (< element 4)))))
    (should
     (eq seg2 (dynaring-dll-find-backwards dll
                                           (lambda (element)
                                             (< element 3)))))
    (should-not
     (dynaring-dll-find-backwards dll
                                  (lambda (element)
                                    nil)))))


(ert-deftest dynaring-dll-delete-segment-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (let ((segment (dynaring-make-segment 1)))
      (should-not (dynaring-dll-delete-segment dll segment))
      (should (dynaring-dll-empty-p dll))))

  ;; 1-element dll
  (with-fixture fixture-1-dll
    (should (dynaring-dll-delete-segment dll segment))
    (should (dynaring-dll-empty-p dll)))

  ;; 2-element dll
  (with-fixture fixture-2-dll
    ;; delete head
    (should (dynaring-dll-delete-segment dll seg1))
    (should (= 1 (dynaring-dll-size dll)))
    (should (eq seg2 (dynaring-dll-head dll)))
    (should-not (dynaring-segment-previous seg2))
    (should-not (dynaring-segment-next seg2)))
  (with-fixture fixture-2-dll
    ;; delete non-head
    (should (dynaring-dll-delete-segment dll seg2))
    (should (= 1 (dynaring-dll-size dll)))
    (should (eq seg1 (dynaring-dll-head dll)))
    (should-not (dynaring-segment-previous seg1))
    (should-not (dynaring-segment-next seg1)))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    ;; delete head
    (should (dynaring-dll-delete-segment dll seg1))
    (should (= 2 (dynaring-dll-size dll)))
    (should (eq seg2 (dynaring-dll-head dll)))
    (should (eq seg3 (dynaring-dll-tail dll)))
    (should (segments-are-linked-p seg2 seg3))
    (should-not (dynaring-segment-previous seg2))
    (should-not (dynaring-segment-next seg3)))
  (with-fixture fixture-3-dll
    ;; delete middle
    (should (dynaring-dll-delete-segment dll seg2))
    (should (= 2 (dynaring-dll-size dll)))
    (should (eq seg1 (dynaring-dll-head dll)))
    (should (eq seg3 (dynaring-dll-tail dll)))
    (should (segments-are-linked-p seg1 seg3))
    (should-not (dynaring-segment-previous seg1))
    (should-not (dynaring-segment-next seg3)))
  (with-fixture fixture-3-dll
    ;; delete tail
    (should (dynaring-dll-delete-segment dll seg3))
    (should (= 2 (dynaring-dll-size dll)))
    (should (eq seg1 (dynaring-dll-head dll)))
    (should (eq seg2 (dynaring-dll-tail dll)))
    (should (segments-are-linked-p seg1 seg2))
    (should-not (dynaring-segment-previous seg1))
    (should-not (dynaring-segment-next seg2))))

(ert-deftest dynaring-dll-delete-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (should-not (dynaring-dll-delete dll 1))
    (should (dynaring-dll-empty-p dll)))

  ;; 1-element dll
  (with-fixture fixture-1-dll
    (should (dynaring-dll-delete dll 1))
    (should (dynaring-dll-empty-p dll)))
  (with-fixture fixture-1-dll
    ;; non-element
    (should-not (dynaring-dll-delete dll 2))
    (should (= 1 (dynaring-dll-size dll))))

  ;; 2-element dll
  (with-fixture fixture-2-dll
    ;; delete head
    (should (dynaring-dll-delete dll 1))
    (should (= 1 (dynaring-dll-size dll)))
    (should (eq seg2 (dynaring-dll-head dll)))
    (should (eq seg2 (dynaring-dll-tail dll))))
  (with-fixture fixture-2-dll
    ;; delete tail
    (should (dynaring-dll-delete dll 2))
    (should (= 1 (dynaring-dll-size dll)))
    (should (eq seg1 (dynaring-dll-head dll)))
    (should (eq seg1 (dynaring-dll-tail dll))))
  (with-fixture fixture-2-dll
    ;; non-element
    (should-not (dynaring-dll-delete dll 3))
    (should (= 2 (dynaring-dll-size dll))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    ;; delete head
    (should (dynaring-dll-delete dll 1))
    (should (= 2 (dynaring-dll-size dll)))
    (should (eq seg2 (dynaring-dll-head dll)))
    (should (eq seg3 (dynaring-dll-tail dll))))
  (with-fixture fixture-3-dll
    ;; delete tail
    (should (dynaring-dll-delete dll 3))
    (should (= 2 (dynaring-dll-size dll)))
    (should (eq seg1 (dynaring-dll-head dll)))
    (should (eq seg2 (dynaring-dll-tail dll))))
  (with-fixture fixture-3-dll
    ;; delete middle
    (should (dynaring-dll-delete dll 2))
    (should (= 2 (dynaring-dll-size dll)))
    (should (eq seg1 (dynaring-dll-head dll)))
    (should (eq seg3 (dynaring-dll-tail dll))))
  (with-fixture fixture-3-dll
    ;; non-element
    (should-not (dynaring-dll-delete dll 4))
    (should (= 3 (dynaring-dll-size dll)))))

(ert-deftest dynaring-dll-contains-p-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (should-not (dynaring-dll-contains-p dll 1)))

  ;; 1-element dll
  (with-fixture fixture-1-dll
    (should (dynaring-dll-contains-p dll 1))
    (should-not (dynaring-dll-contains-p dll 2)))

  ;; 2-element dll
  (with-fixture fixture-2-dll
    (should (dynaring-dll-contains-p dll 1))
    (should (dynaring-dll-contains-p dll 2))
    (should-not (dynaring-dll-contains-p dll 3)))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (should (dynaring-dll-contains-p dll 1))
    (should (dynaring-dll-contains-p dll 2))
    (should (dynaring-dll-contains-p dll 3))
    (should-not (dynaring-dll-contains-p dll 4))))

(ert-deftest dynaring-dll-values-test ()
  ;; empty dll
  (with-fixture fixture-0-dll
    (should (null (dynaring-dll-values dll))))

  ;; 1-element dll
  (with-fixture fixture-1-dll
    (should (equal (list 1) (dynaring-dll-values dll))))

  ;; 2-element dll
  (with-fixture fixture-2-dll
    (should (equal (list 1 2) (dynaring-dll-values dll))))

  ;; 3-element dll
  (with-fixture fixture-3-dll
    (should (equal (list 1 2 3) (dynaring-dll-values dll)))))
