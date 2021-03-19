;;; list-factory.el --- Create lists with modern concise syntax

;; Copyright (C) 2021 Free Software Foundation, Inc.


;; Author: D. L. Nicolai <dalanicolai@gmail.com>
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: lists
;; URL: https://github.com/dalanicolai/list-utils.el

;;; Commentary:

;; This package provides nice things


;;; Code

(defun enumerate (list)
  (let ((i 0)
        l)
    (while list
      (setq l (append l (list (cons i (car list)))))
      (setq list (cdr list))
      (setq i (1+ i)))
    l))

(defvar lf-type)

;; (defun lf-enumerate (list)
;;   (let (l
;;         (i 0))
;;     (dolist (x list l)
;;       (setq l (append l (list (cons i (car list)))))
;;       (setq i (1+ i)))))

(defun lf--range-type (start stop)
  (let ((lf-type (if (eq (type-of start) (type-of stop))
                  (type-of start)
                (error "Elements must be of same type."))))
    lf-type))

(defun lf--range-map (x)
  (pcase lf-type
    ('integer x)
    ('string (string-to-char x))))

(defun lf-range (from to &optional step)
  (let* ((lf-type (lf--range-type from to))
         (beg (lf--range-map from))
         (end (lf--range-map to)))
    (mapcar (lambda (x)
              (pcase lf-type
                ('integer x)
                ('string (char-to-string x))))
            (number-sequence beg end (or step)))))

(defun lf--parse-list (domain-list)
  (if (or (functionp (car domain-list))
          (special-form-p (car domain-list)))
      (eval domain-list)
    domain-list))

(defmacro lf-array (domain)
  (declare (debug (form)))
  `(mapcar (lambda (,(car domain))
             ,(car domain))
           (if (= (length ',(cdr domain)) 1)
               ;; (if (listp ',(cadr domain)) ; required when called directly
               ;;     (lf--parse-list ',(cadr domain))
                 (if (listp ',(eval (cadr domain))) ; required when called from function
                   (lf--parse-list ',(eval (cadr domain)))
                 (if (and (stringp ,(cadr domain)) (> (length ,(cadr domain)) 1))
                     (mapcar #'string ,(cadr domain))
                   (let ((init (if (stringp ,(nth 1 domain))
                                   "a"
                                 1)))
                     (lf-range init ,(cadr domain)))))
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

(defmacro lf--table-f (var fn &rest domain)
  (declare (debug (form form &rest form)))
  `(if (> ,(length domain) 1)
       (mapcan (lambda (,(caar domain))
                 (let ((,(caar domain) ,(caar domain)))
                   (lf--table ,var ,fn ,@(cdr domain))))
               (lf-array ,(car domain)))
     (mapcan (lambda (,(caar domain))
               (let ((,(caar domain) ,(caar domain)))
                 ((lambda (,var) (list ,fn)) ,(caar domain))))
             (lf-array ,(car domain)))))

(defmacro lf-table-f (fn &rest domain)
  (declare (debug (form &rest form)))
  `(lf--table-f ,(caar (last domain)) ,fn ,@domain))

(ert-deftest lf-array-test ()
  (should (equal (lf-array (x 3)) '(1 2 3)))
  (should (equal (lf-array (x "c")) '("a" "b" "c")))
  (should (equal (lf-array (x '(a 2 c))) '(a 2 c)))
  (should (equal (lf-array (x 2 4)) '(2 3 4)))
  (should (equal (lf-array (x "b" "d")) '("b" "c" "d")))
  (should (equal (lf-array (x 1 5 2)) '(1 3 5)))
  (should (equal (lf-array (x "a" "e" 2)) '("a" "c" "e")))

  ;; (should (equal (lf-table a (a "b"))))
  ;; (should (equal (lf-table (concat a b d) (a "b") (b "c") (d "f")) "bcf")))
  )

;; (lf-table a (a "b"))
;; (lf-table (concat a b d) (a "b") (b "c") (d "f"))
;; (lf-table (when (= x 2) "hello") (x 3))
;; (lf-table-f (when (= x 2) "hello") (x 3))
