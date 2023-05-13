;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          random.lisp
;;;; Purpose:       Random Number Generator.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: random.lisp,v 1.5 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(defpackage rsm.random
  (:use :common-lisp)
  (:documentation
   "This package provides a high quality random number generator.

Export Summary:

b-rand: Produces a uniform binary random number. That is its values are 0 or 1.
u-rand: Produces a uniform random number (double-float) on the interval [0,1).
i-rand: Produce a random number on the interval 
        [0, min(2^32, most-positive-fixnum)).
init  : Initialize the random number generator.
set-rand: Set the seed of the random number generator.(takes one arg, a fixnum).
")
  (:export 
   #:b-rand
   #:init
   #:i-rand
   #:u-rand
   #:init/current-time
   #:init/file-path
   ))

(in-package rsm.random)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))

(eval-when (:load-toplevel :compile-toplevel :execute)    
    (uffi:def-function ("urand" u-rand) () 
      :returning :double)
    (uffi:def-function ("brand" b-rand) () 
      :returning :int)
    (uffi:def-function ("random" ii-rand) () 
      :returning :int)
    (uffi:def-function ("rand_init" init) ((x :int)))
    (init 12317))


(defun i-rand ()
  "Return a uniform random number on the interval 
[0, min(2^32, most-positive-fixnum))."
  (mod (ii-rand) most-positive-fixnum))


(defun init/current-time ()
  "Initialize the random number generator using the current time.
The seed will be a value on the interval [0,0xFFFFF)."
  (init 
   (mod (+ (get-universal-time) 
           (get-internal-real-time))
        #16RFFFFF)))


(defun init/file-path (path-name)
  "Initialize the random number generator using a seed from a file.
If the number that is represented in the file is n, then the value of the 
seed will be (mod n 16RFFFFF)."
  (with-open-file (str path-name :direction :input)
    (let ((seed (parse-integer (read str) :junk-allowed t)))
      (if seed
          (init (mod (abs seed) #16RFFFFF))
        (error "init/file-path: Bad seed from path ~s~%" path-name))
      (init seed))))

