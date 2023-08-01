;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-bitcomp.asd
;;;; Purpose:       ASDF Definition File For Package rsm.bitcomp.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-bitcomp.asd,v 1.2 2003/10/03 18:02:12 scottrsm Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.bitcomp.system (:use #:asdf #:cl))
(in-package rsm.bitcomp.system)


(defsystem :rsm-bitcomp
  :name "rsm-bitcomp"
  :author "R. Scott McIntire <scottrsm@gmail.com."
  :version "1.1"
  :maintainer "R. Scott McIntire <scottrsm@gmail.com>."
  :licence "BSD-style"
  :description "Bit Compression."

  :depends-on (rsm-queue)
  
  :components
  ((:file "package")
   (:file "bitcomp" :depends-on ("package"))
   ))


(defsystem :rsm-bitcomp-test
  :depends-on (rsm-bitcomp ptester)
  :components
  ((:file "bitcomp-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-bitcomp-test))))
  (operate 'load-op 'rsm-bitcomp-test)
  (or (funcall (intern (symbol-name '#:run-bitcomp-tests)
		       (find-package 'rsm.bitcomp.test)))
      (error "test-op failed")))
