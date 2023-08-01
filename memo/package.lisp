;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for memoizing functions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.2 2003/09/10 22:19:25 scottrsm Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.memo
  (:use #:cl)
  (:documentation
   "Provide function memoizers and function memo-caching functions of a single
input.

Export Summary:

defmemo : A macro that will define a memoized function.
defcache: A macro that will define a memoized function with a storage limit.
")
  (:export 
   #:defmemo
   #:defcache))
