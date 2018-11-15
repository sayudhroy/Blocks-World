;;;;Lisp Problem Set 2 - Test Cases
;;;;Sayudh Roy

(defun test()

(store-facts '((between a b c) (between c b e) (between b a e)
			(on b a) (on c a) (on a e) (on e b)
			(within a b c) (within x y z)) *kb-ht*)
;; Creating a KNOWLEDGE-BASE in the GLOBAL-HASH-TABLE *KB-HT*

(format t "~%~%KB-HT stores the following entries for running these tests")
(format t " (KEY: VALUE)~%~%")
(loop for key being the hash-keys of *kb-ht* do
(format t "~a: ~a~%" key (gethash key *kb-ht*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1. Function to return a list of tuples of one predicate 'pred'
;;;; when constrained by another predicate 'constraint' at positions
;;;; given by the list entered by the user as 'posns'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%~%Testing function 'CONSTRAIN-PREDICATE':~%~%")

;; Testing Normal Cases

(format t "Input: ~a~%Expected: ((A B C) (B A E))~%Actual: ~a~%~%"
'(constrain-predicate 'between 'on '(3 1))
(constrain-predicate 'between 'on '(3 1)))

(format t "Input: ~a~%Expected: ((C B E))~%Actual: ~a~%~%"
'(constrain-predicate 'between '(not on) '(3 1))
(constrain-predicate 'between '(not on) '(3 1)))

(format t "Input: ~a~%Expected: ((A B C))~%Actual: ~a~%~%"
'(constrain-predicate 'between 'on '(2 1))
(constrain-predicate 'between 'on '(2 1)))

(format t "Input: ~a~%Expected: ((C B E) (B A E))~%Actual: ~a~%~%"
'(constrain-predicate 'between '(not on) '(2 1))
(constrain-predicate 'between '(not on) '(2 1)))

(format t "Input: ~a~%Expected: ((B A E))~%Actual: ~a~%~%"
'(constrain-predicate 'between 'on '(2 3))
(constrain-predicate 'between 'on '(2 3)))

(format t "Input: ~a~%Expected: ((A B C) (C B E))~%Actual: ~a~%~%"
'(constrain-predicate 'between '(not on) '(2 3))
(constrain-predicate 'between '(not on) '(2 3)))

(format t "Input: ~a~%Expected: ((A B C))~%Actual: ~a~%~%"
'(constrain-predicate 'within 'on '(3 1))
(constrain-predicate 'within 'on '(3 1)))

(format t "Input: ~a~%Expected: ((A B C))~%Actual: ~a~%~%"
'(constrain-predicate 'between 'within '(1 2 3))
(constrain-predicate 'between 'within '(1 2 3)))

;; Nil Cases

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(constrain-predicate 'within 'on '(1 1))
(constrain-predicate 'within 'on '(1 1)))

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(constrain-predicate 'between 'within '(3 2 1))
(constrain-predicate 'between 'within '(3 2 1)))

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(constrain-predicate 'between 'within '(2 1))
(constrain-predicate 'between 'within '(2 1)))

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(constrain-predicate 'between 'on '(2))
(constrain-predicate 'between 'on '(2)))

;; Testing Bad Inputs

(format t "Input: ~a" '(constrain-predicate 'between '(not on) '(a b)))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(constrain-predicate 'between '(not on) '(a b)) (format t "~%~%")

(format t "Input: ~a" '(constrain-predicate 'between '(not on) '(a b)))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(constrain-predicate 'between '(not on) '(a b)) (format t "~%~%")

(format t "Input: ~a" '(constrain-predicate 'between '(not on) '(1 2 x)))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(constrain-predicate 'between '(not on) '(1 2 x)) (format t "~%~%")

(format t "Input: ~a" '(constrain-predicate 'touching 'on '(3 1)))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(constrain-predicate 'touching 'on '(3 1)) (format t "~%~%")

(format t "Input: ~a" '(constrain-predicate 'on 'table '(3 1)))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(constrain-predicate 'on 'table '(3 1)) (format t "~%~%")

(format t "Input: ~a" '(constrain-predicate 'between '(not on table) '(3 2)))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(constrain-predicate 'between '(not on table) '(3 2)) (format t "~%~%")

(format t "Input: ~a" '(constrain-predicate 'between '(not table) '(3 2)))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(constrain-predicate 'between '(not table) '(3 2)) (format t "~%~%")

(format t "Input: ~a" '(constrain-predicate 'between '(on table) '(3 2)))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(constrain-predicate 'between '(on table) '(3 2)) (format t "~%~%")

(format t "Input: ~a" '(constrain-predicate '(not between) 'on '(3 2)))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(constrain-predicate '(not between) 'on '(3 2)) (format t "~%~%")

;; Testing Inputs Where No. of Arguments of each tuple of pred is lesser than
;; No. of Arguments of each tuple of constraint

(format t "Input: ~a" '(constrain-predicate 'on 'between '(3 1)))
(format t "~%Expected: No. of Constraint Arguments >")
(format t " No. of Predicate Arguments~%Actual: ")
(constrain-predicate 'on 'between '(3 1)) (format t "~%~%")

(format t "Input: ~a" '(constrain-predicate 'on '(not between) '(3 1)))
(format t "~%Expected: No. of Constraint Arguments >")
(format t " No. of Predicate Arguments~%Actual: ")
(constrain-predicate 'on '(not between) '(3 1)) (format t "~%~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 2. Function to create a new predicate from the first three arguments
;;;; and store it in the knowledge-base hash table with the supplied
;;;; relations along with it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%~%Testing function 'NAME-AND-STORE-RELATION':~%~%")

;; Testing Normal Cases

(format t "Input: ~a~%New Predicate: BETWEEN-ON-31~%Actual: ~a~%~%"
'(name-and-store-relation 'between 'on '(3 1) '((A B C) (B A E)))
(name-and-store-relation 'between 'on '(3 1) '((A B C) (B A E))))

(format t "Input: ~a~%New Predicate: BETWEEN-NOTON-31~%Actual: ~a~%~%"
'(name-and-store-relation 'between '(not on) '(3 1) '((C B E)))
(name-and-store-relation 'between '(not on) '(3 1) '((C B E))))

(format t "Input: ~a~%New Predicate: BETWEEN-ON-21~%Actual: ~a~%~%"
'(name-and-store-relation 'between 'on '(2 1) '((A B C)))
(name-and-store-relation 'between 'on '(2 1) '((A B C))))

(format t "Input: ~a~%New Predicate: WITHIN-ON-31~%Actual: ~a~%~%"
'(name-and-store-relation 'within 'on '(3 1) '((A B C)))
(name-and-store-relation 'within 'on '(3 1) '((A B C))))

(format t "Input: ~a~%New Predicate: BETWEEN-WITHIN-123~%Actual: ~a~%~%"
'(name-and-store-relation 'between 'within '(1 2 3) '((A B C)))
(name-and-store-relation 'between 'within '(1 2 3) '((A B C))))

(format t "Input: ~a~%New Predicate: BETWEEN-NOTON-21~%Actual: ~a~%~%"
'(name-and-store-relation 'between '(not on) '(2 1) '((C B E) (B A E)))
(name-and-store-relation 'between '(not on) '(2 1) '((C B E) (B A E))))

(format t "~%~%KB-HT updated with the new predicates and its relations")
(format t " (KEY: VALUE)~%~%")
(loop for key being the hash-keys of *kb-ht* do
(format t "~a: ~a~%" key (gethash key *kb-ht*)))
(format t "~%~%")

;; Testing Bad Inputs

(format t "Input: ~a" '(name-and-store-relation 'between '(not on) '(a b)
						'((a b c))))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(name-and-store-relation 'between '(not on) '(a b) '((a b c)))
(format t "~%~%")

(format t "Input: ~a" '(name-and-store-relation 'between '(not on) '(3 1)
						'(a b c)))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(name-and-store-relation 'between '(not on) '(3 1) '(a b c))
(format t "~%~%")

(format t "Input: ~a" '(name-and-store-relation 'within '(not on) '1
						'((a b) (b c))))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(name-and-store-relation 'between '(not on) '1 '((a b) (b c)))
(format t "~%~%")

(format t "Input: ~a" '(name-and-store-relation 'within '(on top) '(1 3)
						'((a b) (b c))))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(name-and-store-relation 'between '(on top) '(1 3) '((a b) (b c)))
(format t "~%~%")

(format t "Input: ~a" '(name-and-store-relation 'between '(not on) '(3 1)
						'listOfElements))
(format t "~%Expected: Invalid Input Entered!~%Actual: ")
(name-and-store-relation 'between '(not on) '(3 1) 'listOfElements)

)

 ;;Calling the test function to run all the required tests
(test)