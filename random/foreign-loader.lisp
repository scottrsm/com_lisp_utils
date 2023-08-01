;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          foreign-loader.lisp
;;;; Purpose:       Loads foreign libraries
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: foreign-loader.lisp,v 1.6 2003/09/10 22:19:25 scottrsm Exp $
;;;;
;;;; *************************************************************************

;;; For CMUCL, it's necessary to load foreign files separate from their
;;; usage

(in-package #:cl-user)

(unless (uffi:load-foreign-library 
	 (uffi:find-foreign-library "random" 
				    (list
				     (pathname-directory *load-truename*)
                                     "/Shared/lisp/util/random/"
				     "/usr/lib/rsm-random/")
                                    :types '("so" "dll")
                                    :drive-letters '("C" "D" "E"))
	 :supporting-libraries '("c")
	 :module "rsm.random")
  (warn "Unable to load rsm.random library"))

