;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          problems1.lisp
;;;; Purpose:       Example Definition Of Genetic Algorithm Problems.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: problems1.lisp,v 1.5 2003/09/14 16:54:27 rscottmcintire Exp $
;;;; *************************************************************************


(in-package rsm.genetic-alg)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;; Genetic algorithm problem (1).
(defgenetic max-alt-ones
  
  ;; Mutation rate - percentage of mutations which occur during mating.
  :mutation-rate 5
  
  ;; Fitness function - a function: Maximum fitness goes to genes 
  ;; that alternate between ones and zeros.
  ;; NOTE: A gene is a vector from the alphabet listed below.
  :fitness-function #'(lambda (gene) 
                        (let ((sum 10))
                          (let ((last (aref gene 0)))
                            (loop for i from 1 below (length gene) do
                                  (let ((val (aref gene i)))
                                    (unless (= val last)
                                      (incf sum 10))
                                    (setf last val))))
                          sum))
  
  ;; Gene alphabet - a vector.
  :alphabet #(0 1)

  
  ;; Initial gene pool - a list of genes (each gene is a list with
  ;; elements taken from the alphabet.
  :pool '((1 1 1 0 0 0 1 0) 
          (0 0 1 1 0 0 1 1) 
          (0 0 0 0 1 1 1 1)
          (1 1 1 0 0 0 1 0) 
          (0 0 1 1 0 0 1 1) 
          (0 0 1 0 1 1 0 1)
          (1 1 1 0 0 0 1 0) 
          (0 0 0 1 0 0 1 1) 
          (0 1 0 0 1 0 0 1)
          (0 0 0 1 0 1 1 0)))


;; Genetic algorithm problem (2).
(defgenetic max-ones 
  
  ;; Mutation rate - percentage of mutations which occur during mating.
  :mutation-rate 5  
  
  ;; Fitness function - a function: Fitness function favors genes 
  ;; with more ones.
  :fitness-function #'(lambda (gene) 
                        (let ((sum 10))
                          (+ sum (loop for val across gene sum val))))
  
  ;; Gene alphabet - a vector.
  :alphabet #(0 1)
  
  ;; Initial gene pool - a list of genes (each gene is a list with
  ;; elements taken from the alphabet.)
  :pool '((0 1 0 1 0 1 0 1) 
          (1 0 1 0 1 0 1 0) 
          (1 1 1 0 0 0 1 0) 
          (0 0 1 1 0 0 1 1) 
          (0 0 0 0 1 1 1 1)
          (1 1 1 0 0 0 1 0) 
          (0 0 1 1 0 0 1 1) 
          (0 0 1 0 1 1 1 1)
          (1 1 1 0 0 0 1 0) 
          (0 0 0 1 0 0 1 1) 
          (0 1 0 0 1 0 1 1)
          (0 0 0 1 0 1 1 0)))

