;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          example.lisp
;;;; Purpose:       Fuzzy Examples.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: example.lisp,v 1.4 2004/02/17 03:05:12 rscottmcintire Exp $
;;;; *************************************************************************

;;; NOTE: Requires the package rsm.fuzzy.

(in-package #:rsm.fuzzy)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;; Clear out any old systems.
(clear-fuzzy-systems)


(deffuzzy my-fuzzy
    :adjs
  ((small ((1 0) (3 0.25) (4 0.75) (6 1) (8 0)))
   (large ((1 0) (2 0.30) (4 0.80) (5 1) (7 0)))
   (blue ((1 0) (1.5 0.4) (3 0.5) (5 1) (7 0)))
   (green ((1 0) (2 0.1) (4 1) (6.5 0)))
   (dry  ((1 0) (2.5 0.50) (3.5 0.75) (4.5 1) (6.5 0)))
   (wet ((1 0) (3 0.50) (4 0.75) (5 1) (6.5 0))))
  :adj-groups 
  ((size (small large))
   (color (blue green))
   (humidity (dry wet)))
  :vars
  ((x 1.0 :adj-group size)
   (y 2.0 :adj-group color)
   (z 3.0 
      :adj-group humidity
      :rules ((r1 IF (AND (x IS somewhat small) 
                          (y IS very blue)    ) THEN dry) 
              (r2 IF (AND (x IS not large) 
                          (y IS very green)   ) THEN wet)))))



(deffuzzy my-fuzzy2
    :adjs
  ((small  (tri 1 3 5))
   (medium (tri 3 5 6))
   (large  (tri 5 7 8))
   (blue   (tri 1 3 5))
   (green  (trap 2 4 5 7))
   (dry    (trap 1 3.5 5.5 7))
   (wet    (trap 2.5 5.5 7 8)))
  :adj-groups 
  ((size (small medium large))
   (color (blue green))
   (humidity (dry wet)))
  :vars
  ((x 3.0 :adj-group size)
   (y 4.0 :adj-group color)
   (z 3.5 
      :adj-group humidity
      :rules ((r1 IF (AND 
                      (or
                       (x IS somewhat small) 
                       (x IS medium))
                      (y IS very blue)        ) THEN dry)
              (r2 IF (AND (x IS not large) 
                          (y IS very green)   ) THEN wet)))))


;;; Fuzzy system describing the control of an inverted pendulum.
;;; The control fuzzy variable, v and its rules are used to control
;;; the dynamics of the inverted pendulum to keep it straight up.
;;; The rules for the controller, v, are found from page 320 of 
;;; "Neural Networks AND Fuzzy Systems" by Bart Kosko.
(deffuzzy inverted-pendulum
    :adjs
  ;; Size group adjectives - adjectives for variables theta, dtheta, and v.
  ;; nl - Negative Large
  ;; nm - Negative Medium
  ;; ns - Negative Small
  ;; ze - Zero
  ;; ps - Positive Small
  ;; pm - Positive Medium
  ;; pl - Positive Large
  ;;
  ((:range ((nl nm ns ze ps pm pl) -1.0 1.0 0.25)))
  :adj-groups 
  ((size (nl nm ns ze ps pm pl)))
  :vars
  ((theta 0.1 :adj-group size)
   (dtheta 0.1 :adj-group size)
   (v 0.0 :adj-group size
      
      ;; Rules for the angular acceleration control, v based on 
      ;; the current angle, theta, of the pendulum, AND the 
      ;; angular rate of the pendulum, dtheta.
      :rules 
      ((r1 IF (AND (theta IS ze)
                   (dtheta IS nl)) THEN pl)
       (r2 IF (AND (theta IS ze)
                   (dtheta IS nm)) THEN pm)
       (r3 IF (AND (theta IS ze)
                   (dtheta IS ns)) THEN ps)
       (r4 IF (AND (theta IS ze)
                   (dtheta IS ze)) THEN ze)
       (r5 IF (AND (theta IS ze)
                   (dtheta IS ps)) THEN ns)
       (r6 IF (AND (theta IS ze)
                   (dtheta IS pm)) THEN nm)
       (r7 IF (AND (theta IS ze)
                   (dtheta IS pl)) THEN nl)
       (r8 IF (AND (theta IS nl)
                   (dtheta IS ze)) THEN pl)
       (r9 IF (AND (theta IS nm)
                   (dtheta IS ze)) THEN pm)
       (r10 IF (AND (theta IS ns)
                    (dtheta IS ze)) THEN ps)
       (r11 IF (AND (theta IS ps)
                    (dtheta IS ze)) THEN ns)
       (r12 IF (AND (theta IS pm)
                    (dtheta IS ze)) THEN nm)
       (r13 IF (AND (theta IS pl)
                    (dtheta IS ze)) THEN nl)
       (r14 IF (AND (theta IS ps)
                    (dtheta IS ns)) THEN ns)
       (r15 IF (AND (theta IS ns)
                    (dtheta IS ps)) THEN ps))))
  
  ;; The dynamics here are interpreted as:
  ;;   theta  := theta + dtheta * dt
  ;;   dtheta := dtheta + k * v * dt
  ;; with the value of v determined by firing its rules at each 
  ;; time step.
  ;; theta is the angle of the pendulum. 0 degrees is
  ;; straight up, and a positive angle means a clockwise rotation.
  ;; dtheta represents the rate of change of the angle.
  ;; Finally, v is the acceleration (the control). 
  ;; Use (rsm.fuzzy:run-dynamics begin-time end-time) to run 
  ;; a simulation of the system. This function returns a list 
  ;; of the time step (incremented by dt) followed by a list of 
  ;; the values of theta, dtheta, and v at that time step.
  :dynamics
  ((:time-info (time (dt 0.01))) ; The time variable and the time increment.
   (:constants ((k 5.0)))
   (:controls (v))
   ;; Consists of a list of variables and their update forms.
   (:equations
    ;; theta'   = dtheta
    ;; detheta' = k * v
    ((<- theta  (* dtheta dt))
     (<- dtheta (* v k dt)))))
  )



;;;; EXAMPLE:
;; Write out a list of the form: (time (theta dtheta v)) 
;; for the controlled inverted pendulum 
;; fuzzy system above.
(set-fuzzy-system inverted-pendulum)
(format t "~s~%" (run-dynamics 0.0 5.0))


