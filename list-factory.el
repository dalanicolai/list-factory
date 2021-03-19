;;; list-factory.el --- Frobnicate and bifurcate flanges

;; Copyright (C) 2021 Free Software Foundation, Inc.


;; Author: D. L. Nicolai <dalanicolai@gmail.com>
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: tools, lists
;; URL: https://github.com/dalanicolai/list-utils.el

;;; Commentary:

;; This package provides nice things

;; (defun enumerate (list)
;;   (let ((i 0)
;;         l)
;;     (while list
;;       (setq l (append l (list (cons i (car list)))))
;;       (setq list (cdr list))
;;       (setq i (1+ i)))
;;     l))

(defun lf-enumerate (list)
  (let (l
        (i 0))
    (dolist (x list l)
      (setq l (append l (list (cons i (car list)))))
      (setq i (1+ i)))))

(defun lf--range-type (start stop)
  (let ((type (if (eq (type-of start) (type-of stop))
                  (type-of start)
                (error "Elements must be of same type."))))
    type))

(defun lf--range-map (x)
  (pcase type
    ('integer x)
    ('string (string-to-char x))))

(defun lf-range (from to &optional step)
  (let* ((type (lf--range-type from to))
         (beg (lf--range-map from))
         (end (lf--range-map to)))
    (mapcar (lambda (x)
              (pcase type
                ('integer x)
                ('string (char-to-string x))))
            (number-sequence beg end (or step)))))

(lf-range 1 10 2)
(lf-range "a" "z" 2)

;;; for loop from manual
;; the following function is taken from section 14.5.3 of the elisp manual
;; (defmacro for (var from init to final do &rest body)
;;   "Execute a simple for loop: (for i from 1 to 10 do (print i)).
;; See Info node `(elisp) Surprising Local Vars'"
;;   (let ((tempvar (make-symbol "max")))
;;     `(let ((,var ,init)
;;            (,tempvar ,final))
;;        (while (<= ,var ,tempvar)
;;          ,@body
;;          (inc ,var)))))

(defun lf--parse-list (domain-list)
  (if (functionp (car domain-list))
      (eval domain-list)
    domain-list))

(defmacro lf-array (domain)
  `(mapcar (lambda (,(car domain))
             ,(car domain))
           (if (= (length ',(cdr domain)) 1)
               (if (listp ',(cadr domain))
                   (lf--parse-list ',(cadr domain))
                 (let ((init (if (stringp ,(nth 1 domain))
                                 "a"
                               1)))
                   (lf-range init ,(cadr domain))))
             (lf-range ,(cadr domain) ,(caddr domain) (or ,(cadddr domain))))))

(defmacro lf--table (var fn &rest domain)
  (declare (debug (form form &rest form)))
  `(if (> ,(length domain) 1)
       (mapcan (lambda (,(caar domain))
                 (let ((,(caar domain) ,(caar domain)))
                   (lf--table ,var ,fn ,@(cdr domain))))
               (lf-array ,(car domain)))
     (mapcar (lambda (,(caar domain))
               (let ((,(caar domain) ,(caar domain)))
                 ((lambda (,var) ,fn) ,(caar domain))))
             (lf-array ,(car domain)))))

(defmacro lf-table (fn &rest domain)
  (declare (debug (form &rest form)))
  `(lf--table ,(caar (last domain)) ,fn ,@domain))

(lf-table (concat a b d) (a "b") (b "c") (d "f"))

(ert-deftest lf-array-test ()
  (should (equal (lf-array (x 3)) '(1 2 3)))
  (should (equal (lf-array (x "c")) '("a" "b" "c")))
  (should (equal (lf-array (x (a 2 c))) '(a 2 c)))
  (should (equal (lf-array (x 2 4)) '(2 3 4)))
  (should (equal (lf-array (x "b" "d")) '("b" "c" "d")))
  (should (equal (lf-array (x 1 5 2)) '(1 3 5)))
  (should (equal (lf-array (x "a" "e" 2)) '("a" "c" "e")))
  )

;; (defmacro lf--table (fn &rest domain)
;;   (declare (debug (form form &rest form)))
;;   `(if (> ,(length domain) 1)
;;        (mapcan (lambda (,(caar domain))
;;                  (let ((,(caar domain) ,(caar domain)))
;;                    (lf--table ,fn ,@(cdr domain))))
;;                (lf-range ,(cadar domain) ,(caddar domain)))
;;      (mapcar (lambda (,(caar domain))
;;                (let ((,(caar domain) ,(caar domain)))
;;                  ((lambda (y) (* x y)) ,(caar domain))))
;;              (lf-range ,(cadar domain) ,(caddar domain)))))

;; (lf--table x (x 1 3) (y 2 4))
