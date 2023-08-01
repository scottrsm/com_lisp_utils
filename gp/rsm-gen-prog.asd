;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-gen-prog.asd
;;;; Purpose:       ASDF Definition File For Package rsm.gen-prog.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-gen-prog.asd,v 1.2 2003/09/10 22:19:25 scottrsm Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.gen-prog.system (:use #:asdf #:cl))
(in-package rsm.gen-prog.system)


(defsystem :rsm-gen-prog
  :name "rsm-gen-prog"
  :author "R. Scott McIntire <scottrsm@gmail.com>."
  :version "1.0"
  :maintainer "R. Scott McIntire <scottrsm@gmail.com>."
  :licence "BSD-style"
  :description "Genetic Programming."

  :depends-on (rsm-queue rsm-cache rsm-rand)
  
  :components
  ((:file "package")
   (:file "gen-prog" :depends-on ("package"))
   ))


(defsystem :rsm-gen-prog-test
  :depends-on (rsm-gen-prog ptester)
  :components
  ((:file "gen-prog-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-gen-prog-test))))
  (operate 'load-op 'rsm-gen-prog-test)
  (or (funcall (intern (symbol-name '#:run-gen-prog-tests)
		       (find-package 'rsm.gen-prog.test)))
      (error "test-op failed")))
