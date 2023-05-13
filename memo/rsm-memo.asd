;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-memo.asd
;;;; Purpose:       ASDF Definition File For Package rsm.memo.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-memo.asd,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.memo.system (:use #:asdf #:cl))
(in-package rsm.memo.system)


(defsystem :rsm-memo
  :name "rsm-memo"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Memoizing functions"
    
  :components
  ((:file "package")
   (:file "memo" :depends-on ("package"))
   ))


(defsystem :rsm-memo-test
  :depends-on (rsm-memo ptester)
  :components
  ((:file "memo-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-memo-test))))
  (operate 'load-op 'rsm-memo-test)
  (or (funcall (intern (symbol-name '#:run-memo-tests)
		       (find-package 'rsm.memo.test)))
      (error "test-op failed")))
