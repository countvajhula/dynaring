
;; Add source paths to load path so the tests can find the source files
;; Adapted from:
;; https://github.com/Lindydancer/cmake-font-lock/blob/47687b6ccd0e244691fb5907aaba609e5a42d787/test/cmake-font-lock-test-setup.el#L20-L27
(defvar dynamic-ring-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".."))
  (add-to-list 'load-path
               (concat dynamic-ring-test-setup-directory dir)))

;;

(require 'dynamic-ring)

(ert-deftest make-dyn-ring-test ()
  (should (make-dyn-ring)))

(ert-deftest dyn-ring-empty-p-test ()
  (should (dyn-ring-empty-p (make-dyn-ring)))
  (let ((ring (make-dyn-ring)))
    (dyn-ring-insert ring 1)
    (should-not (dyn-ring-empty-p ring))))

(ert-deftest dyn-ring-size-test ()
  (should (= 0 (dyn-ring-size (make-dyn-ring))))
  (let ((ring (make-dyn-ring)))
    (dyn-ring-insert ring 1)
    (should (= 1 (dyn-ring-size ring)))))

(ert-deftest dyn-ring-value-test ()
  (should (null (dyn-ring-value (make-dyn-ring))))
  (let ((ring (make-dyn-ring)))
    (dyn-ring-insert ring 1)
    (should (= 1 (dyn-ring-value ring)))))

(ert-deftest dyn-ring-element-test ()
  (should (dyn-ring-make-element 1))
  (should (= 1
             (dyn-ring-element-value
              (dyn-ring-make-element 1))))
  (let ((elem (dyn-ring-make-element 1)))
    (dyn-ring-set-element-value elem 2)
    (should (= 2
               (dyn-ring-element-value elem))))
  (let* ((ring (make-dyn-ring))
         (elem (dyn-ring-insert ring 1)))
    (should (null (dyn-ring-element-prev elem)))
    (should (null (dyn-ring-element-next elem))))
  (let* ((ring (make-dyn-ring))
         (elem (dyn-ring-insert ring 1))
         (elem2 (dyn-ring-insert ring 2)))
    (should (equal elem2 (dyn-ring-element-prev elem)))
    (should (equal elem2 (dyn-ring-element-next elem)))
    (should (equal elem (dyn-ring-element-prev elem2)))
    (should (equal elem (dyn-ring-element-next elem2)))))