;;;;Lisp Problem Set 0 - Test Cases
;;;;Sayudh Roy

(defun test()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;1. Function to return t if item is in list, else returns nil

(format t "TESTING FUNCTION 'OCCUR-IN'~%~%")

;;Given Examples
(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" '(occur-in 'x 'x) (occur-in 'x 'x))
(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" '(occur-in '(a b) '((a b) c)) 
						(occur-in '(a b) '((a b) c)))
(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" '(occur-in '(a b) '(a b c)) 
						(occur-in '(a b) '(a b c)))
(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" '(occur-in 'x '((f x) y (((x z) ())))) 
						(occur-in 'x '((f x) y (((x z) ())))))
(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" '(occur-in "this" "this") 
						(occur-in "this" "this"))
(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" '(occur-in "this" "this and that") 
						(occur-in "this" "this and that"))
(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" '(occur-in #\i '((#\t #\h #\i #\s) and that)) 
						(occur-in #\i '((#\t #\h #\i #\s) and that)))
(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" '(occur-in 3 3.14159265) (occur-in 3 3.14159265))
(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" '(occur-in 3 '(three (3) blind mice)) 
						(occur-in 3 '(three (3) blind mice)))
(terpri)
(terpri)

;;Long Examples which will return true
(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" '(occur-in 'x '(3 2 (y a) (w (c (x a v) () d w)))) 
						(occur-in 'x '(3 2 (y a) (w (c (x a v) () d w)))))
(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" '(occur-in '(1 2) '((2 1) 3 (4 (5 6)) (1 2))) 
						(occur-in '(1 2) '((2 1) 3 (4 (5 6)) (1 2))))
(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" '(occur-in "violet" '("green" "yellow" "violet")) 
						(occur-in "violet" '("green" "yellow" "violet")))
(terpri)
(terpri)

;;Long Examples which will return false
(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" '(occur-in '(a b c) '(a b c (b a) (a (b d ())))) 
						(occur-in '(a b c) '(a b c (b a) (a (b d ())))))
(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" '(occur-in "red" '("red and blue" "pink" (1 2))) 
						(occur-in "red" '("red and blue" "pink" (1 2))))
(terpri)
(terpri)
(terpri)
(terpri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;2. Function to differentiate single-variable monomial

(format t "TESTING FUNCTION 'DERIV1'~%~%")

;;Given Examples
(format t "Input: ~a~%Expected: (6 X 2)~%Actual: ~a~%~%" '(deriv1 '(2 x 3)) (deriv1 '(2 x 3)))
(format t "Input: ~a~%Expected: (2 X -2)~%Actual: ~a~%~%" '(deriv1 '(-2 y -1)) (deriv1 '(-2 x -1)))
(format t "Input: ~a~%Expected: (2.0 Z 3)~%Actual: ~a~%~%" '(deriv1 '(0.5 z 4)) (deriv1 '(0.5 z 4)))
(format t "Input: ~a~%Expected: (7 X 0)~%Actual: ~a~%~%" '(deriv1 '(7 x 1)) (deriv1 '(7 x 1)))
(terpri)
(terpri)

;;Examples which will yield (0 x 0)
(format t "Input: ~a~%Expected: (0 X 0)~%Actual: ~a~%~%" '(deriv1 '(0 x 5)) (deriv1 '(0 x 5)))
(format t "Input: ~a~%Expected: (0 X 0)~%Actual: ~a~%~%" '(deriv1 '(2 x 0)) (deriv1 '(2 x 0)))
(format t "Input: ~a~%Expected: (0 X 0)~%Actual: ~a~%~%" '(deriv1 '(0 x 0)) (deriv1 '(0 x 0)))
(terpri)
(terpri)

;;Examples with invalid inputs
(format t "Input: ~a~%Expected: Error! Wrong Input Type!~%Actual: ~a~%~%" '(deriv1 '(2 x y 3)) (deriv1 '(2 x y 3)))
(format t "Input: ~a~%Expected: Error! Wrong Input Type!~%Actual: ~a~%~%" '(deriv1 '(a x 5)) (deriv1 '(a x 5)))
(format t "Input: ~a~%Expected: Error! Wrong Input Type!~%Actual: ~a~%~%" '(deriv1 '(3 x b)) (deriv1 '(3 x b)))
(format t "Input: ~a~%Expected: Error! Wrong Input Type!~%Actual: ~a~%~%" '(deriv1 '(a 4 5)) (deriv1 '(a 4 5)))
(terpri)
(terpri)
(terpri)
(terpri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;3. Function to find the list of atoms comprising the leaves of the tree

(format t "TESTING FUNCTION 'TREE-YIELD'~%~%")

;;Given Examples
(format t "Input: ~a~%Expected: A~%Actual: ~a~%~%" '(tree-yield 'a) (tree-yield 'a))
(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" '(tree-yield nil) (tree-yield nil))
(format t "Input: ~a~%Expected: (A B)~%Actual: ~a~%~%" '(tree-yield '(a b)) (tree-yield '(a b)))
(format t "Input: ~a~%Expected: (A B C D E F)~%Actual: ~a~%~%" '(tree-yield '((a b) c (d (e f)))) 
						(tree-yield '((a b) c (d (e f)))))
(format t "Input: ~a~%Expected: (A B)~%Actual: ~a~%~%" '(tree-yield '(a.b)) (tree-yield '(a . b)))
(format t "Input: ~a~%Expected: (A.B :C)~%Actual: " '(tree-yield '((a.b) (:c)))) (print (tree-yield '((a.b) (:c))))(terpri)(terpri)
(format t "Input: ~a (Please Check File For Unformatted Input) ~%Expected: (\#\\a \"bb\" |;| SETQ X NIL)~%Actual: " 
'(tree-yield '((#\a "bb") |;| (setq x ())))) 
						(print (tree-yield '((#\a "bb") |;| (setq x ()))))(terpri)(terpri)
(terpri)
(terpri)

;;Longer Examples
(format t "Input: ~a~%Expected: (1 2 3 4 5 NIL 6 7 NIL 8 9 10 NIL 11)~%Actual: ~a~%~%" '(tree-yield '(1 (2 3) (4 (5 () 6) (7 () 8 (9)
(10 ()) 11))))(tree-yield '(1 (2 3) (4 (5 () 6) (7 () 8 (9) (10 ()) 11)))))
(format t "Input: ~a (Please Check File For Unformatted Input)~%Expected: (\"this\" \"this and that\" \"how\" \"are\" \"you\" 1 2)~%Actual: " 
		'(tree-yield '("this" "this and that" "how" "are" "you" (1 2))))
		(print (tree-yield '("this" "this and that" "how" "are" "you" (1 2))))(terpri)(terpri)
(terpri)
(terpri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;4. Function that finds the first match of a predicate to an element of the facts list

(format t "TESTING FUNCTION 'find-match'~%~%")

;;Testing with following inputs
(format t "Testing With A Regular Input Finding a Match~%")
(format t "Entered Predicate: (on ?x ?y)~%Entered Facts: ((on-table b1)(on-table b2)(on b1)(on b1 b2))~%")
(format t "Expected: ((?X B1)(?Y B2))~%Actual: ")
(print (find-match '(on ?x ?y) '((on-table b1)(on-table b2)(on b1)(on b1 b2))))(terpri)(terpri)

(format t "Testing With A Predicate of Zero Arguments~%")
(format t "Entered Predicate: (rainy-weather)~%Entered Facts: ((sunny-weather w1)(rainy-weather r1)(rainy-weather)(on b1 b2))~%")
(format t "Expected: T~%Actual: ")
(print (find-match '(rainy-weather) '((sunny-weather w1)(rainy-weather r1)(rainy-weather)(on b1 b2))))(terpri)(terpri)

(format t "Testing With An Input Without a Match~%")
(format t "Entered Predicate: (on ?x ?y ?z)~%Entered Facts: ((on-table w1)(on r1 r2)(in r1 r2 r3)(on))~%")
(format t "Expected: NIL~%Actual: ")
(print (find-match '(on ?x ?y ?z) '((on-table w1)(on r1 r2)(in r1 r2 r3)(on))))(terpri)(terpri)

(format t "Testing With A Random Input~%")
(format t "Entered Predicate: (touching (right-side of B1) (right-side of B2))~%Entered Facts: ((in-between b1 b2)(on r1 r2)(touching r1 r2))~%")
(format t "Expected: (((RIGHT-SIDE OF B1) R1) ((RIGHT-SIDE OF B2) R2))~%Actual: ")
(print (find-match '(touching (right-side of B1) (right-side of B2)) '((in-between b1 b2)(on r1 r2)(touching r1 r2))))(terpri)(terpri)

(format t "Testing With An Input which has facts with the same predicate to show it returns the first one~%")
(format t "Entered Predicate: (on ?x ?y)~%Entered Facts: ((on-table b1)(on b7 b8)(on b1)(on b3 b4))~%")
(format t "Expected: ((?X B7)(?Y B8))~%Actual: ")
(print (find-match '(on ?x ?x) '((on-table b1)(on b7 b8)(on b1)(on b3 b4))))(terpri)(terpri)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;5. Functions that	i) store a fact into two entries of the hash table
;;;;						a) Stores the predicate as the key and the variables as the value
;;;;						b) Stores the entire fact as a key and 'T' as the value
;;;;					ii) stores facts into the hash table by calling the first function
;;;;						using the mapcar functionality

(format t "TESTING FUNCTION 'store-facts'~%~%")

;;Testing with following inputs
(format t "Testing with updating the hash table with a series of facts first:~%")
(format t "Entered Facts: ((on-table a1)(on-table a2)(on b1)(on b3 b4)(in-between c1 c2)(on b4 b5)(in-between c3 c4))~%")
(format t "Input: ~a" '(store-facts ((on-table a1) (on-table a2) (on b1) (on b3 b4) (in-between c1 c2) (on b4 b5) (in-between c3 c4)) ht))
(store-facts '((on-table a1) (on-table a2) (on b1) (on b3 b4) (in-between c1 c2) (on b4 b5) (in-between c3 c4)) ht)

;;Using the gethash function now to retrieve the values corresponding the keys
(format t "~%~%Now testing with the gethash function:~%")
(format t "~%~%Input: (gethash ~a ht)~%" 'on)
(format t "Expected: (B1 B3 B4 B4 B5)~%Actual: ")
(print (gethash 'on ht))

(format t "~%~%Input: (gethash ~a ht)~%" 'on-table)
(format t "Expected: (A1 A2)~%Actual: ")
(print (gethash 'on-table ht))

(format t "~%~%Input: (gethash ~a ht)~%" 'in-between)
(format t "Expected: (C1 C2 C3 C4)~%Actual: ")
(print (gethash 'in-between ht))

(format t "~%~%Input: (gethash ~a ht)~%" '(in-between c1 c2))
(format t "Expected: T~%Actual: ") ; Checking with one of the facts with the entire fact being the predicate
									; It is supposed to return 'T' as it has been designed previously
(print (gethash '(in-between c1 c2) ht))

(format t "~%~%Input: (gethash ~a ht)~%" '(on b3)) ;Does not match any value in the hash table
(format t "Expected: NIL~%Actual: ")
(print (gethash '(on b3) ht))

(format t "~%~%Input: (gethash ~a ht)~%" 'b1) ;Checking with a value stored in the hash table
												; It is supposed to return NIL as the hashtable compares keys
												; to find the match
(format t "Expected: NIL~%Actual: ")
(print (gethash 'b1 ht))

)
 ;;Calling the test function to run all the required tests
(test)