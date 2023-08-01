;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-rsa.asd
;;;; Purpose:       ASDF Definition File For Package rsm.rsa.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-rsa.asd,v 1.8 2003/10/21 21:00:32 scottrsm Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.rsa.system (:use #:asdf #:cl))
(in-package rsm.rsa.system)


(defsystem rsm-rsa
  :name "rsm-rsa"
  :author "R. Scott McIntire <scottrsm@gmail.com>."
  :version "1.3"
  :maintainer "R. Scott McIntire <scottrsm@gmail.com>."
  :licence "BSD-style"
  :description "RSA encryption utilities."

  :depends-on (rsm-mod)
  
  :components
  ((:file "package")
   (:file "rsa" :depends-on ("package"))
   ))


(defsystem :rsm-rsa-test
  :depends-on (rsm-rsa ptester)
  :components
  ((:file "rsa-test")))


(defsystem :rsm-rsa-examples
  :depends-on (rsm-rsa rsm-string)
  :components
  ((:file "rsa-examples")))

   

(defmethod perform ((o test-op) (c (eql (find-system 'rsm-rsa-test))))
  (operate 'load-op 'rsm-rsa-test)
  (or (funcall (intern (symbol-name '#:run-rsa-tests)
		       (find-package 'rsm.rsa.test)))
      (error "test-op failed")))

