;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-queue.asd
;;;; Purpose:       ASDF Definition File For Package rsm.queue.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-queue.asd,v 1.6 2003/10/23 15:43:31 scottrsm Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.queue.system (:use #:asdf #:cl))
(in-package rsm.queue.system)


(defsystem :rsm-queue
  :name "rsm-queue"
  :author "R. Scott McIntire <scottrsm@gmail.com>."
  :version "1.2"
  :maintainer "R. Scott McIntire <scottrsm@gmail.com>."
  :licence "BSD-style"
  :description "Queing functions."
    
  :components
  ((:file "package")
   (:file "queue" :depends-on ("package"))
   ))


(defsystem :rsm-queue-test
  :depends-on (rsm-queue ptester)
  :components
  ((:file "queue-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-queue-test))))
  (operate 'load-op 'rsm-queue-test)
  (or (funcall (intern (symbol-name '#:run-queue-tests)
		       (find-package 'rsm.queue.test)))
      (error "test-op failed")))
