(require 's)
(require 'seq)

(defun parse-range (s)
  (let ((parts (s-split "-" s)))
    (list (string-to-number (car parts))
          (string-to-number (car (cdr parts))))))

(ert-deftest parse-range-test ()
  (should (equal (parse-range "1-3") '(1 3))))

(defun parse-line (line)
  (let* ((parts (s-split "," line))
        (range-1 (parse-range (car parts)))
        (range-2 (parse-range (car (cdr parts)))))
    (list range-1 range-2)))

(ert-deftest parse-line-test ()
  (should (equal (parse-line "1-2,3-4") '((1 2) (3 4)))))

(defun pairs-from-file (path)
  (let ((s (with-temp-buffer
              (insert-file-contents path)
              (buffer-string)))
         (pairs))

    (dolist (el (s-split "\n" s))
      (unless (string-empty-p el)
        (setq pairs (cons (parse-line el) pairs))))
    pairs))

(defun range-contains (a b)
  (let ((a0 (car a))
        (a1 (car (cdr a)))
        (b0 (car b))
        (b1 (car (cdr b))))
    (and (<= a0 b0)
         (<= b1 a1))))

(ert-deftest range-contains-test-1 ()
  (should (range-contains '(1 10) '(3 4))))

(ert-deftest range-contains-test-2 ()
  (should-not (range-contains '(1 10) '(30 50))))

(defun range-overlaps (a b)
  (let ((a0 (car a))
        (a1 (car (cdr a)))
        (b0 (car b))
        (b1 (car (cdr b))))
    (or (and (<= a0 b0) (<= b0 a1))
        (and (<= b0 a0) (<= a0 b1)))))

(ert-deftest range-overlaps-test-1 ()
  (should (range-overlaps '(1 10) '(5 50))))

(ert-deftest range-overlaps-test-2 ()
  (should-not (range-overlaps '(1 10) '(40 50))))

(defun count-overlapping (pairs)
  (seq-count (lambda (pair) (range-overlaps (car pair) (car (cdr pair)))) pairs))

(defun count-containing (pairs)
  (seq-count
   (lambda (pair)
     (let ((a (car pair))
           (b (car (cdr pair))))
       (or (range-contains a b)
           (range-contains b a))))
   pairs))

  
(let ((pairs (pairs-from-file "./day04.txt")))
  (print (count-containing pairs))
  (print (count-overlapping pairs)))

  
