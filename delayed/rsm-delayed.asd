;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-delayed.asd
;;;; Purpose:       ASDF Definition File For Package rsm.delayed.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-delayed.asd,v 1.2 2003/09/10 22:19:24 scottrsm Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.delayed.system (:use #:asdf #:cl))
(in-package rsm.delayed.system)


(defsystem :rsm-delayed
  :name "rsm-delayed"
  :author "R. Scott McIntire <scottrsm@gmail.com>."
  :version "1.0"
  :maintainer "R. Scott McIntire <scottrsm@gmail.com>."
  :licence "BSD-style"
  :description "Delayed lists."

  :depends-on (rsm-queue rsm-filter)
  
  :components
  ((:file "package")
   (:file "delayed" :depends-on ("package"))
   ))


(defsystem :rsm-delayed-test
  :depends-on (rsm-delayed ptester)
  :components
  ((:file "delayed-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-delayed-test))))
  (operate 'load-op 'rsm-delayed-test)
  (or (funcall (intern (symbol-name '#:run-delayed-tests)
		       (find-package 'rsm.delayed.test)))
      (error "test-op failed")))
