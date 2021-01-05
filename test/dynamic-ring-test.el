
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
(require 'cl)

(defun segments-are-linked-p (previous next)
  (and (eq (dyn-ring-segment-next previous) next)
       (eq (dyn-ring-segment-previous next) previous)))

(defun segment-is-free-p (segment)
  (and (null (dyn-ring-segment-next segment))
       (null (dyn-ring-segment-previous segment))))

(ert-deftest dyn-ring-test ()
  ;; null constructor
  (should (make-dyn-ring))

  ;; dyn-ring-empty-p
  (should (dyn-ring-empty-p (make-dyn-ring)))
  (let ((ring (make-dyn-ring)))
    (dyn-ring-insert ring 1)
    (should-not (dyn-ring-empty-p ring)))

  ;; dyn-ring-size
  (should (= 0 (dyn-ring-size (make-dyn-ring))))
  (let ((ring (make-dyn-ring)))
    (dyn-ring-insert ring 1)
    (should (= 1 (dyn-ring-size ring))))

  ;; dyn-ring-head
  (should (null (dyn-ring-head (make-dyn-ring))))
  (let* ((ring (make-dyn-ring))
         (elem (dyn-ring-insert ring 1)))
    (should (equal elem (dyn-ring-head ring))))

  ;; dyn-ring-value
  (should (null (dyn-ring-value (make-dyn-ring))))
  (let ((ring (make-dyn-ring)))
    (dyn-ring-insert ring 1)
    (should (= 1 (dyn-ring-value ring)))))

(ert-deftest dyn-ring-segment-test ()
  ;; constructor
  (should (dyn-ring-make-segment 1))

  ;; dyn-ring-segment-value
  (should (= 1
             (dyn-ring-segment-value
              (dyn-ring-make-segment 1))))

  ;; dyn-ring-set-segment-value
  (let ((elem (dyn-ring-make-segment 1)))
    (dyn-ring-set-segment-value elem 2)
    (should (= 2
               (dyn-ring-segment-value elem))))

  ;; dyn-ring-segment-previous and dyn-ring-segment-next
  (let* ((ring (make-dyn-ring))
         (head (dyn-ring-insert ring 1)))
    (should (eq head (dyn-ring-segment-previous head)))
    (should (eq head (dyn-ring-segment-next head))))
  (let* ((ring (make-dyn-ring))
         (elem (dyn-ring-insert ring 1))
         (elem2 (dyn-ring-insert ring 2)))
    (should (eq elem2 (dyn-ring-segment-previous elem)))
    (should (eq elem2 (dyn-ring-segment-next elem)))
    (should (eq elem (dyn-ring-segment-previous elem2)))
    (should (eq elem (dyn-ring-segment-next elem2)))))

(ert-deftest dyn-ring-traverse-test ()
  ;; empty ring
  (let ((ring (make-dyn-ring))
        (memo (list)))
    (letf ((memofn (lambda (arg)
                     (push arg memo))))
      (should-not (dyn-ring-traverse ring memofn))
      (should (null memo))))

  ;; one-element ring
  (let* ((ring (make-dyn-ring))
         (memo (list)))
    (letf ((memofn (lambda (arg)
                     (push arg memo))))
      (dyn-ring-insert ring 1)
      (should (dyn-ring-traverse ring memofn))
      (should (equal memo (list 1)))))

  ;; two-element ring
  (let* ((ring (make-dyn-ring))
         (memo (list)))
    (letf ((memofn (lambda (arg)
                     (push arg memo))))
      (dyn-ring-insert ring 1)
      (dyn-ring-insert ring 2)
      (should (dyn-ring-traverse ring memofn))
      (should (equal memo (list 1 2)))))

  ;; 3-element ring
  (let* ((ring (make-dyn-ring))
         (memo (list)))
    (letf ((memofn (lambda (arg)
                     (push arg memo))))
      (dyn-ring-insert ring 1)
      (dyn-ring-insert ring 2)
      (dyn-ring-insert ring 3)
      (should (dyn-ring-traverse ring memofn))
      (should (equal memo (list 1 2 3))))))

(ert-deftest dyn-ring-traverse-collect-test ()
  ;; empty ring
  (let ((ring (make-dyn-ring)))
    (let ((result (dyn-ring-traverse-collect ring #'1+)))
      (should (null result))))

  ;; one-element ring
  (let ((ring (make-dyn-ring)))
    (dyn-ring-insert ring 1)
    (let ((result (dyn-ring-traverse-collect ring #'1+)))
      (should (equal result (list 2)))))

  ;; two-element ring
  (let* ((ring (make-dyn-ring)))
    (dyn-ring-insert ring 1)
    (dyn-ring-insert ring 2)
    (let ((result (dyn-ring-traverse-collect ring #'1+)))
      (should (equal result (list 2 3)))))

  ;; 3-element ring
  (let* ((ring (make-dyn-ring)))
    (dyn-ring-insert ring 1)
    (dyn-ring-insert ring 2)
    (dyn-ring-insert ring 3)
    (let ((result (dyn-ring-traverse-collect ring #'1+)))
      (should (equal result (list 2 3 4))))))

(ert-deftest dyn-ring-insert-test ()
  ;; empty ring
  (let ((ring (make-dyn-ring)))
    (should (dyn-ring-insert ring 1))
    (should (= 1 (dyn-ring-value ring)))
    (let ((head (dyn-ring-head ring)))
      (should (segments-are-linked-p head head))))

  ;; one-element ring
  (let* ((ring (make-dyn-ring))
         (elem1 (dyn-ring-insert ring 1)))
    (let ((new (dyn-ring-insert ring 2)))
      (should new)
      (should (= 2 (dyn-ring-value ring)))
      (should (segments-are-linked-p elem1 new))
      (should (segments-are-linked-p new elem1))))

  ;; two-element ring
  (let* ((ring (make-dyn-ring))
         (elem1 (dyn-ring-insert ring 1))
         (elem2 (dyn-ring-insert ring 2)))
    (let ((new (dyn-ring-insert ring 3)))
      (should new)
      (should (= 3 (dyn-ring-value ring)))
      (should (segments-are-linked-p elem1 new))
      (should (segments-are-linked-p new elem2))
      (should (segments-are-linked-p elem2 elem1)))))

(ert-deftest dyn-ring-rotate-test ()
  ;; empty ring
  (let ((ring (make-dyn-ring)))
    (should (null (dyn-ring-rotate-left ring)))
    (should (null (dyn-ring-rotate-right ring))))

  ;; 1-element ring
  (let* ((ring (make-dyn-ring))
         (segment (dyn-ring-insert ring 1)))
    (should (eq segment (dyn-ring-rotate-left ring)))
    (should (eq segment (dyn-ring-rotate-right ring))))

  ;; 2-element ring
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2)))
    (should (eq seg1 (dyn-ring-rotate-left ring)))
    (should (eq seg2 (dyn-ring-rotate-left ring)))
    (should (eq seg1 (dyn-ring-rotate-right ring)))
    (should (eq seg2 (dyn-ring-rotate-right ring))))

  ;; 3-element ring
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    (should (eq seg1 (dyn-ring-rotate-left ring)))
    (should (eq seg2 (dyn-ring-rotate-left ring)))
    (should (eq seg3 (dyn-ring-rotate-left ring)))
    (should (eq seg2 (dyn-ring-rotate-right ring)))
    (should (eq seg1 (dyn-ring-rotate-right ring)))
    (should (eq seg3 (dyn-ring-rotate-right ring)))))

(ert-deftest dyn-ring-delete-test ()
  ;; empty ring
  (let ((ring (make-dyn-ring))
        (segment (dyn-ring-make-segment 1)))
    (should (null (dyn-ring-delete ring segment)))
    (should (dyn-ring-empty-p ring)))

  ;; 1-element ring
  (let* ((ring (make-dyn-ring))
         (segment (dyn-ring-insert ring 1)))
    (should (dyn-ring-delete ring segment))
    (should (dyn-ring-empty-p ring)))

  ;; 2-element ring
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2)))
    ;; delete head
    (should (dyn-ring-delete ring seg2))
    (should (= 1 (dyn-ring-size ring)))
    (should (eq seg1 (dyn-ring-head ring)))
    (should (segments-are-linked-p seg1 seg1)))
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2)))
    ;; delete non-head
    (should (dyn-ring-delete ring seg1))
    (should (= 1 (dyn-ring-size ring)))
    (should (eq seg2 (dyn-ring-head ring)))
    (should (segments-are-linked-p seg2 seg2)))

  ;; 3-element ring
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    ;; delete head
    (should (dyn-ring-delete ring seg3))
    (should (= 2 (dyn-ring-size ring)))
    (should (eq seg2 (dyn-ring-head ring)))
    (should (segments-are-linked-p seg2 seg1))
    (should (segments-are-linked-p seg1 seg2)))
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    ;; delete right
    (should (dyn-ring-delete ring seg2))
    (should (= 2 (dyn-ring-size ring)))
    (should (eq seg3 (dyn-ring-head ring)))
    (should (segments-are-linked-p seg3 seg1))
    (should (segments-are-linked-p seg1 seg3)))
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    ;; delete left
    (should (dyn-ring-delete ring seg1))
    (should (= 2 (dyn-ring-size ring)))
    (should (eq seg3 (dyn-ring-head ring)))
    (should (segments-are-linked-p seg3 seg2))
    (should (segments-are-linked-p seg2 seg3))))

(ert-deftest dyn-ring-destroy-test ()
  ;; empty ring
  (let ((ring (make-dyn-ring)))
    (should-not (dyn-ring-destroy ring)))

  ;; 1-element ring
  (let* ((ring (make-dyn-ring))
         (segment (dyn-ring-insert ring 1)))
    (should (dyn-ring-destroy ring))
    (should (null (dyn-ring-head ring)))
    (should (= 0 (dyn-ring-size ring)))
    (should (segment-is-free-p segment)))

  ;; 2-element ring
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2)))
    (should (dyn-ring-destroy ring))
    (should (null (dyn-ring-head ring)))
    (should (= 0 (dyn-ring-size ring)))
    (should (segment-is-free-p seg1))
    (should (segment-is-free-p seg2)))

  ;; 3-element ring
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    (should (dyn-ring-destroy ring))
    (should (null (dyn-ring-head ring)))
    (should (= 0 (dyn-ring-size ring)))
    (should (segment-is-free-p seg1))
    (should (segment-is-free-p seg2))
    (should (segment-is-free-p seg3))))

(ert-deftest dyn-ring-rotate-until-test ()
  ;; empty ring
  (let ((ring (make-dyn-ring)))
    (should-not (dyn-ring-rotate-until ring
                                       #'dyn-ring-rotate-left
                                       (lambda (element)
                                         t))))

  ;; 1-element ring
  (let* ((ring (make-dyn-ring))
         (segment (dyn-ring-insert ring 1)))
    (should (dyn-ring-rotate-until ring
                                   #'dyn-ring-rotate-left
                                   (lambda (element)
                                     t)))
    (should (eq segment (dyn-ring-head ring))))
  (let* ((ring (make-dyn-ring))
         (segment (dyn-ring-insert ring 1)))
    (should-not (dyn-ring-rotate-until ring
                                       #'dyn-ring-rotate-left
                                       (lambda (element)
                                         nil)))
    (should (eq segment (dyn-ring-head ring))))

  ;; 2-element ring
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2)))
    (should (dyn-ring-rotate-until ring
                                   #'dyn-ring-rotate-left
                                   (lambda (element)
                                     t)))
    (should (eq seg1 (dyn-ring-head ring))))
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2)))
    (should (dyn-ring-rotate-until ring
                                   #'dyn-ring-rotate-right
                                   (lambda (element)
                                     t)))
    (should (eq seg1 (dyn-ring-head ring))))

  ;; 3-element ring
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    (should (dyn-ring-rotate-until ring
                                   #'dyn-ring-rotate-left
                                   (lambda (element)
                                     t)))
    (should (eq seg1 (dyn-ring-head ring))))
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    (should (dyn-ring-rotate-until ring
                                   #'dyn-ring-rotate-right
                                   (lambda (element)
                                     t)))
    (should (eq seg2 (dyn-ring-head ring))))
  ;; non-trivial predicate
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    (should (dyn-ring-rotate-until ring
                                   #'dyn-ring-rotate-left
                                   (lambda (element)
                                     (= element 2))))
    (should (eq seg2 (dyn-ring-head ring))))
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    (should (dyn-ring-rotate-until ring
                                   #'dyn-ring-rotate-right
                                   (lambda (element)
                                     (= element 2))))
    (should (eq seg2 (dyn-ring-head ring))))
  ;; predicate never satisfied
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    (should-not (dyn-ring-rotate-until ring
                                       #'dyn-ring-rotate-left
                                       (lambda (element)
                                         nil)))
    (should (eq seg3 (dyn-ring-head ring))))
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    (should-not (dyn-ring-rotate-until ring
                                       #'dyn-ring-rotate-right
                                       (lambda (element)
                                         nil)))
    (should (eq seg3 (dyn-ring-head ring)))))

(ert-deftest dyn-ring-find-test ()
  ;; empty ring
  (let ((ring (make-dyn-ring)))
    (should-not (dyn-ring-find ring (lambda (element) t))))

  ;; 1-element ring
  (let* ((ring (make-dyn-ring))
         (segment (dyn-ring-insert ring 1)))
    (should (equal (list 1)
                   (dyn-ring-find ring
                                  (lambda (element)
                                    (= 1 element)))))
    (should-not (dyn-ring-find ring
                               (lambda (element)
                                 nil))))

  ;; 2-element ring
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2)))
    (should (equal (list 1)
                   (dyn-ring-find ring
                                  (lambda (element)
                                    (= 1 element)))))
    (let ((result (dyn-ring-find ring
                                 (lambda (element)
                                   (> element 0)))))
      (should (member 1 result))
      (should (member 2 result)))
    (should-not (dyn-ring-find ring
                               (lambda (element)
                                 nil))))

  ;; 3-element ring
  (let* ((ring (make-dyn-ring))
         (seg1 (dyn-ring-insert ring 1))
         (seg2 (dyn-ring-insert ring 2))
         (seg3 (dyn-ring-insert ring 3)))
    (should (equal (list 1)
                   (dyn-ring-find ring
                                  (lambda (element)
                                    (= 1 element)))))
    (let ((result (dyn-ring-find ring
                                 (lambda (element)
                                   (> element 1)))))
      (should (member 2 result))
      (should (member 3 result))
      (should-not (member 1 result)))
    (should-not (dyn-ring-find ring
                               (lambda (element)
                                 nil)))))
