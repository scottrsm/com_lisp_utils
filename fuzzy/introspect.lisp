;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          introspect.lisp
;;;; Purpose:       Fuzzy System Introspection Facilities.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: introspect.lisp,v 1.4 2004/02/17 03:05:12 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.fuzzy)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;; INSPECTION

(defun clear-fuzzy-systems ()
  "Remove all the fuzzy systems."
  (clrhash *fuzzy-hash*))

(defmacro find-fuzzy-system (sym)
  "Find a fuzzy system associated to symbol, <sym>."
  `(gethash ,(symbol-name sym) *fuzzy-hash*))

(defmacro set-fuzzy-system (sym)
  "Set the current fuzzy system to the one associated with symbol, <sym>."
  `(setf *fuzzy-sys* (gethash ,(symbol-name sym) *fuzzy-hash*)))

(defmacro get-var-val (sym &key (fuzzy-sys *fuzzy-sys*))
  "Get the value of fuzzy variable, <sym>."
  `(var-value (find-var ',sym :fuzzy-sys ,fuzzy-sys)))

(defmacro set-var-val (sym val &key (fuzzy-sys *fuzzy-sys*))
  "Set the value of fuzzy variable, <sym>."
  `(setf (var-value (find-var ',sym :fuzzy-sys ,fuzzy-sys)) ,val))

(defun set-var-val-f (sym val &key (fuzzy-sys *fuzzy-sys*))
  "Set the value of fuzzy variable, <sym>."
  (setf (var-value (find-var sym :fuzzy-sys fuzzy-sys)) val))

(defmacro find-adj (sym &key (fuzzy-sys *fuzzy-sys*))
  "Find the fuzzy adjective associated with the symbol, <sym>."
  `(gethash ,(symbol-name sym) ,(adj-hash fuzzy-sys)))

(defun find-var (sym &key (fuzzy-sys *fuzzy-sys*))
  "Find the fuzzy variable associated with the symbol, <sym>."
  (gethash (symbol-name sym) (var-hash fuzzy-sys)))

(defmacro find-rule (rule-sym var-sym &key (fuzzy-sys *fuzzy-sys*))
  "Find the fuzzy rule associated with symbol, <fule-sym>, owned by 
the fuzzy variable associated with the symbol, <var-sym>."
  `(gethash ,(symbol-name rule-sym) 
            (rule-hash (find-var ',var-sym :fuzzy-sys ,fuzzy-sys))))

(defun find-adj-group (name &key (fuzzy-sys *fuzzy-sys*))
  "Find the fuzzy adjective group with name, <name>."
  (gethash name (adj-group-hash fuzzy-sys)))

(defun print-vars (&key (fuzzy-sys *fuzzy-sys*))
  "Print out all the fuzzy variables."
  (with-standard-io-syntax
    (maphash #'(lambda (sym var) 
                 (format t "~%var = ~a: " sym) 
                 (print-object var *standard-output*)) (var-hash fuzzy-sys)))
  (values))

(defun print-adjs (&key (fuzzy-sys *fuzzy-sys*))
  "Print out all the fuzzy adjectives."
  (with-standard-io-syntax
    (maphash #'(lambda (sym adj) 
                 (format t "~%adj = ~a: " sym) 
                 (print-object adj *standard-output*)) (adj-hash fuzzy-sys)))
  (values))


(defun get-var (sym &key (fuzzy-sys *fuzzy-sys*))
  "Associates a symbol with a fuzzy variable object."
  (gethash (symbol-name sym) (var-hash fuzzy-sys)))

(defun get-adj (sym &key (fuzzy-sys *fuzzy-sys*))
  "Associates a symbol with a fuzzy adjective object."
  (gethash (symbol-name sym) (adj-hash fuzzy-sys)))

(defun get-adj-group (sym &key (fuzzy-sys *fuzzy-sys*))
  "Associates a symbol with a fuzzy adjective object."
  (gethash (symbol-name sym) (adj-group-hash fuzzy-sys)))

