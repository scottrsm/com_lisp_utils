;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.mod.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          fuzzy-test.lisp
;;;; Purpose:       Regression testing for Fuzzy Systems.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Feb 2004
;;;;
;;;; $Id: fuzzy-test.lisp,v 1.1 2004/02/17 03:05:12 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.fuzzy.test
  (:use #:cl #:ptester)
  (:documentation
   "Provides a test harness for fuzzy systems.")
  )


(in-package rsm.fuzzy.test)


;;;; INFRASTRUCTURE FUNCTIONS AND VARIABLES.

(clear-fuzzy-systems)

(setf rsm.fuzzy::*fuzzy-sys* 
    (make-instance 'rsm.fuzzy::standard-fuzzy-system))

(defvar *tolerance* 1.0d-5
  "The tolerance used to decide floating point equality.")

(defun float-equal (x y)
  "Equal if the relative error of x and y is less than +tolerance+.
When +tolerance+ is 1.0d-4, then equality means that <x> and <y> 
are equal with a relative error of a hundredth of a percent."
  (if (and (= 0.0d0 x) (= 0.0d0 y))
      t
    (let ((max (max (abs x) (abs y))))
      (< (/ (abs (- x y)) max) *tolerance*))))


(defparameter *test-adj1* 
    (rsm.fuzzy::make-adj 'adj1 '((2.0 0.0) (4.0 1.0) (5.0 1.0) (7.0 0.0))))

(defparameter *test-adj2* 
    (rsm.fuzzy::make-adj 'adj2 '((-2.0 0.0) (-1.0 1.0) (2.0 1.0) (3.0 0.0))))

(defparameter *test-adj3* 
    (rsm.fuzzy::make-adj 'adj3 '((-2.0 0.0) (-1.0 0.7) (2.0 0.7) (3.0 0.0))))

;;;; RUN THE TESTS.


(defun run-fuzzy-tests ()

  (rsm.fuzzy::clear-fuzzy-systems)

  (with-tests (:name "FUZZY TESTS")

    (test 11.8125
          (rsm.fuzzy::cut-moment *test-adj1* 0.75)
          :fail-info "Test 1"
          :test #'float-equal)
    
    (test 3.0
          (rsm.fuzzy::cut-area *test-adj1* 1.0)
          :fail-info "Test 2"
          :test #'float-equal)
    
    (test 2.52
          (rsm.fuzzy::cut-area *test-adj1* 0.7)
          :fail-info "Test 3"
          :test #'float-equal)
    
    (test 4.0
          (rsm.fuzzy::cut-area *test-adj2* 1.0)
          :fail-info "Test 4"
          :test #'float-equal)
    
    (test 3.01
          (rsm.fuzzy::cut-area *test-adj2* 0.7)
          :fail-info "Test 5"
          :test #'float-equal)
    
    (test 2.0
          (rsm.fuzzy::cut-moment *test-adj2* 1.0)
          :fail-info "Test 6"
          :test #'float-equal)
    
    (test 1.505
          (rsm.fuzzy::cut-moment *test-adj2* 0.7)
          :fail-info "Test 7"
          :test #'float-equal)
    
    (test 2.8
          (rsm.fuzzy::cut-area *test-adj3* 1.0)
          :fail-info "Test 8"
          :test #'float-equal)
    
    (test 2.8
          (rsm.fuzzy::cut-area *test-adj3* 0.7)
          :fail-info "Test 9"
          :test #'float-equal)
    
    (test 1.4
          (rsm.fuzzy::cut-moment *test-adj3* 1.0)
          :fail-info "Test 10"
          :test #'float-equal)
    
    (test 1.4
          (rsm.fuzzy::cut-moment *test-adj3* 0.7)
          :fail-info "Test 11"
          :test #'float-equal)
    )
  
  (rsm.fuzzy::clear-fuzzy-systems)
  
  t
  )

