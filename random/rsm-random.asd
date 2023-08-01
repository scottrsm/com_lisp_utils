;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: rsm.random.system -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-random.asd
;;;; Purpose:       ASDF System Definition For Loading/Testing rsm.random
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id: rsm-random.asd,v 1.3 2003/09/10 22:19:25 scottrsm Exp $
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:rsm.random.system (:use #:asdf #:cl))
(in-package #:rsm.random.system)

(defsystem rsm-random
    :depends-on (uffi)
    :components
    ((:file "foreign-loader")
     (:file "random" :depends-on ("foreign-loader"))))

(defmethod perform ((o test-op) (c (eql (find-system 'rsm-random))))
  (operate 'load-op 'rsm-random-test)
  (operate 'test-op 'rsm-random-test :force t))

(defsystem rsm-random-test
    :depends-on (rsm-random ptester)
    :components
    ((:file "random-test")))

(defmethod perform ((o test-op) (c (eql (find-system 'rsm-random-test))))
  (operate 'load-op 'rsm-random-test)
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:rsm.random.test)))
      (error "test-op failed")))
