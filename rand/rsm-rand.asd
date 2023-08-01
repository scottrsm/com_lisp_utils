;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-rand.asd
;;;; Purpose:       ASDF Definition File For Package rsm.rand.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-rand.asd,v 1.5 2003/10/20 02:26:33 scottrsm Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.rand.system (:use #:asdf #:cl))
(in-package rsm.rand.system)


(defsystem :rsm-rand
  :name "rsm-rand"
  :author "R. Scott McIntire <scottrsm@gmail.com>."
  :version "1.2"
  :maintainer "R. Scott McIntire <scottrsm@gmail.com>."
  :licence "BSD-style"
  :description "Discrete random number generator."
    
  :components
  ((:file "package")
   (:file "rand" :depends-on ("package"))
   ))


(defsystem :rsm-rand-test
  :depends-on (rsm-rand ptester)
  :components
  ((:file "rand-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-rand-test))))
  (operate 'load-op 'rsm-rand-test)
  (or (funcall (intern (symbol-name '#:run-rand-tests)
		       (find-package 'rsm.rand.test)))
      (error "test-op failed")))
