;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-web-finance.asd
;;;; Purpose:       ASDF Definition File For Package rsm.web-finance.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id: rsm-web-finance.asd,v 1.3 2003/10/03 17:57:19 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.webfinance.system (:use #:asdf #:cl))
(in-package rsm.webfinance.system)

#+allegro (require 'aserve)


(defsystem :rsm-web-finance
  :name "rsm-web-finance"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.1"
  :maintainer "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :licence "BSD-style"
  :description "Web Interface For Financial Loan Utilities."

  :depends-on (rsm-finance)
  :components
  ((:file "web-finance")
   ))

#-allegro (asdf:operate 'asdf:load-op 'aserve) ;; to find and load
                                               ;; paserve's aserve.asd file

