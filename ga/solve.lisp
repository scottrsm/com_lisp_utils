;;;;
;;;; SOLVE THE SAMPLE PROBLEMS.
;;;;


;;;; problems1.lisp

;;; Clear out the old problems.
(rsm.genetic-alg:clear-genetic-problems)

;;; Compile and load a list of genetic problem definitions from file 
;;; problems1.lisp.
(compile-file "problems1.lisp")
(load "problems1")

(format t "~%SOLVE ALL PROBLEMS (in problem set 1):~%~%")
;;; Solve the genetic problems by running a simulation that evolves
;;; the gene pool of each problem 500 times.
(rsm.genetic-alg:display-solutions 
 (rsm.genetic-alg:solve-all-genetic-problems 500))

(format t "~%SOLVE JUST \"MAX_ONES\" (in problem set 1):~%~%")

;;; Or, just solve the problem named "max-ones".
(rsm.genetic-alg:display-solution 
 (rsm.genetic-alg:solve-genetic-problem 500 "max-ones"))


;;;; problems2.lisp

;;; Clear out the old problems.
(rsm.genetic-alg:clear-genetic-problems)

;;; Compile and load a list of genetic problem definitions from file 
;;; problems2.lisp.
(compile-file "problems2.lisp")
(load "problems2")

(format t "~%SOLVE ALL PROBLEMS (in problem set 2):~%~%")

;;; Solve the genetic problem. Run 1500 evolution steps; the 
;;; genes are allowed to vary in size; the 
;;; mutation rate is 10%. 
;;; Display the best 3 solutions found of all the gene pools produced.
(rsm.genetic-alg:display-solutions 
 (rsm.genetic-alg:solve-all-genetic-problems 
  1500 
  :gene-length-constant? nil 
  :mutation-rate 10 
  :k 3))
