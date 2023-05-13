;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          problems2.lisp
;;;; Purpose:       Example Definition Of Genetic Algorithm Problems.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: problems2.lisp,v 1.4 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************


(in-package rsm.genetic-alg)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))

;; Genetic algorithm problem.
(defgenetic obstacle-avoid          
  
  ;; Mutation rate - percentage of mutations which occur during mating.
  :mutation-rate 5  
  
  ;; Use the letters of the gene as instructions
  ;; to move an object on a two dimensional surface.
  ;; The successful genes will move the object 
  ;; (which starts at (0,0)) as close as they can
  ;; to (8,8) while avoiding an obstacle (a square [3,4] x [3,4])
  ;; and keeping the gene length (instruction length) small.
  ;; NOTE: A gene is a vector from the alphabet listed below.
  :fitness-function #'(lambda (gene) 
                        (let ((pos-x 0) (pos-y 0) 
                              (fit 100))
                          
                          (loop for ch across gene do
                            (case ch
                              
                              ;; Move one unit in the positive x direction.
                              (0 (incf pos-x 1))
                              
                              ;; Move one unit in the negative x direction.
                              (1 (decf pos-x 1))
                                 
                              ;; Move one unit in the positive y direction.
                              (2 (incf pos-y 1))
                              
                              ;; Move one unit in the negative y direction.
                              (3 (decf pos-y 1)))
                            
                            ;; If in the obstacle area, take a penalty.
                            (when (and
                                   (<= pos-x 4)
                                   (>= pos-x 3)
                                   (<= pos-y 4)
                                   (>= pos-y 3))
                              (decf fit 30)))
                          
                          ;; Longer genes take a penalty.
                          (decf fit (* 0.5 (length gene)))
                          
                          ;; Genes farther away from (8,8) take a penalty.
                          (let ((mis-x (- 8 pos-x)) (mis-y (- 8 pos-y)))
                            (decf fit (* 2 (+ (* mis-x mis-x) 
                                              (* mis-y mis-y))))
                          
                            ;; Return the fitness - make sure it's non-negative.
                            (max 10 fit))))
  
  ;; Gene alphabet - a vector of instructions.
  :alphabet #(0 1 2 3)
  
  ;; Initial gene pool - a list of genes (each gene is a list with
  ;; elements taken from the alphabet). Each gene is a list of 
  ;; describing how a hypothetical object is to move in the plane.
  :pool '((0 1 2 3 3 2 3 2 0 2 1)
          (2 1 2 1 0 2 3 2 3 2 0)
          (3 0 1 2 0 2 0 0 3 1 0)
          (2 0 2 1 0 2 0 1 0 1 0)
          (1 0 0 0 2 3 2 3 1 2 3)
          (2 0 1 0 2 0 3 3 1 2 0)
          (1 0 1 0 2 2 3 3 1 3 0)
          (2 0 1 3 2 0 3 3 1 0 0)
          (1 0 0 0 3 0 2 3 0 2 0)
          (1 0 1 0 2 2 3 3 0 3 0)
          (3 0 1 0 2 0 3 0 2 0 0)))

