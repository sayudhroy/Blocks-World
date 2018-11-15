;;;;Lisp Problem Set 1 - Test Cases
;;;;Sayudh Roy

;; Creating hash table for function 'store-fact'
(defparameter ht (make-hash-table :test #'equal))

(defun test()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1. Function to return a list of constants for a list of atomic facts.

(format t "TESTING FUNCTION 'EXTRACT-CONSTANTS'~%~%")

(format t "Input: ~a~%Expected: (A B TABLE)~%Actual: ~a~%~%"
'(extract-constants '((block a)(block b)(red a)(blue b)(on a b)(on b table)))
(extract-constants '((block a)(block b)(red a)(blue b)(on a b)(on b table))))

(format t "Input: ~a~%Expected: (B C A)~%Actual: ~a~%~%"
'(extract-constants '((in-between a b)(behind b)(rainy)(on c)(at c a)))
(extract-constants '((in-between a b)(behind b)(rainy)(on c)(at c a))))

(format t "Input: ~a~%Expected: (A B C D E F G)~%Actual: ~a~%~%"
'(extract-constants '((on a b)(on b c)(on c d)(on d e)(on e f)(on f g)))
(extract-constants '((on a b)(on b c)(on c d)(on d e)(on e f)(on f g))))

(format t "Input: ~a~%Expected: (C B E D A)~%Actual: ~a~%~%"
'(extract-constants '((predicate-for A B C B A D E D A)))
(extract-constants '((predicate-for A B C B A D E D A))))

(format t "Input: ~a~%Expected: (1 2 3 4 5 6)~%Actual: ~a~%~%"
'(extract-constants '((count 1 2 3 4 5 6)))
(extract-constants '((count 1 2 3 4 5 6))))

;; FACTS WITH THE SAME CONSTANT FOR DIFFERENT PREDICATES
(format t "Input: ~a~%Expected: (UR)~%Actual: ~a~%~%"
'(extract-constants '((at ur)(about ur)(on ur)(in ur)(what ur)))
(extract-constants '((at ur)(about ur)(on ur)(in ur)(what ur))))

;; REPEATED FACTS
(format t "Input: ~a~%Expected: (A B C D E F)~%Actual: ~a~%~%"
'(extract-constants '((on a b)(on c d)(on a b)(on c d)(on e f)))
(extract-constants '((on a b)(on c d)(on a b)(on c d)(on e f))))

;; FACTS WITH ONLY PREDICATES
(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(extract-constants '((on)(on-top)(between)(in)(at)(the)))
(extract-constants '((on)(on-top)(between)(in)(at)(the))))

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(extract-constants '((a)(b)(c)(d)(e)(f)))
(extract-constants '((a)(b)(c)(d)(e)(f))))

;; EMPTY FACTS
(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(extract-constants '(()()()()()()))
(extract-constants '(()()()()()())))

(terpri)
(terpri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 2. Function to store a fact in a hash-table under different keys.

(format t "TESTING FUNCTION 'STORE-FACT'~%~%")

(format t "Input: ~a~%Expected Keys In Hash-Table: (ON (ON A B) (ON A -) (ON - B))~%Actual Keys In Hash Table: ~a~%~%"
'(store-fact '(on a b) ht)(store-fact '(on a b) ht))

(format t "Input: ~a~%Expected Keys In Hash-Table: (ON (ON A B) (ON A -) (ON - B) (ON A B C) (ON A - -) (ON - B -) (ON - - C))~%Actual Keys In Hash Table: ~a~%~%"
'(store-fact '(on a b c) ht)(store-fact '(on a b c) ht))

(format t "Input: ~a~%Expected Keys In Hash-Table:  (ON (ON A B) (ON A -) (ON - B) (ON A B C) (ON A - -) (ON - B -) (ON - - C) (ON A B C D) (ON A - - -) (ON - B - -) (ON - - C -) (ON - - - D))~%Actual Keys In Hash Table: ~a~%~%"
'(store-fact '(on a b c d) ht)(store-fact '(on a b c d) ht))

;; Using the gethash function now to retrieve the values
;; corresponding the keys
(format t "~%~%NOW TESTING WITH THE GETHASH FUNCTION:~%")
(format t "~%Input: (gethash '~a ht)~%" 'on)
(format t "Expected: (ON A B ON A B C ON A B C D)~%Actual: ~a~%"
(gethash 'on ht))

(format t "~%Input: (gethash '~a ht)~%" '(on a b))
(format t "Expected: (ON A B)~%Actual: ~a~%"
(gethash '(on a b) ht))

(format t "~%Input: (gethash '~a ht)~%" '(on a - -))
(format t "Expected: (ON A B C)~%Actual: ~a~%"
(gethash '(on a - -) ht))

(format t "~%Input: (gethash '~a ht)~%" '(on - - c))
(format t "Expected: (ON A B C)~%Actual: ~a~%"
(gethash '(on - - c) ht))

(format t "~%Input: (gethash '~a ht)~%" '(on - b - -))
(format t "Expected: (ON A B C D)~%Actual: ~a~%"
(gethash '(on - b - -) ht))

;; Testing for Keys which will do not exist

(format t "~%Input: (gethash '~a ht)~%" '(on a - c))
(format t "Expected: NIL~%Actual: ~a~%"
(gethash '(on a - c) ht))

(format t "~%Input: (gethash '~a ht)~%" '(on a b c -))
(format t "Expected: NIL~%Actual: ~a~%~%~%"
(gethash '(on a b c -) ht))
 
 ;; Adding a fact which already exists in the hash-table

 (format t "Input: ~a~%Expected Keys In Hash-Table: (ON (ON A B) (ON A -) (ON - B) (ON A B C) (ON A - -) (ON - B -) (ON - - C) (ON A B C D) (ON A - - -) (ON - B - -) (ON - - C -) (ON - - - D))~%Actual Keys In Hash Table: ~a~%~%"
'(store-fact '(on a b) ht)(store-fact '(on a b) ht))

(format t "Input: ~a~%Expected Keys In Hash-Table: (ON (ON A B) (ON A -) (ON - B) (ON A B C) (ON A - -) (ON - B -) (ON - - C) (ON A B C D) (ON A - - -) (ON - B - -) (ON - - C -) (ON - - - D))~%Actual Keys In Hash Table: ~a~%~%"
'(store-fact 'on ht)(store-fact 'on ht))

(terpri)
(terpri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3. Function to store facts in a hash-table and extracts constants from
;;;; those facts and holds it a global variable domain.

(format t "TESTING FUNCTION 'STORE-FACTS'~%~%")

(format t "Creating a Hash-Table 'HT'.~%~%")
(defparameter ht (make-hash-table :test #'equal))

(format t "Testing with updating the hash table with a series of facts first:~%")
(format t "Entered Facts: ((on-table a1)(on-table a2)(on b1)(on b3 b4)(in-between c1 c2)(on b4 b5)(in-between c3 c4))~%")
(format t "Input: ~a~%List of Constants In Domain: ~a"
'(store-facts ((on-table a1) (on-table a2) (on b1) (on b3 b4) (in-between c1 c2) (on b4 b5) (in-between c3 c4)) ht)
(store-facts '((on-table a1) (on-table a2) (on b1) (on b3 b4) (in-between c1 c2) (on b4 b5) (in-between c3 c4)) ht))

(format t "~%~%Resetting the Hash-Table 'HT'.~%~%")
(defparameter ht (make-hash-table :test #'equal))

(format t "Testing with facts with similar individual constants to check for redundancy in domain list:~%")
(format t "Entered Facts: ((on a b) (on a b c) (on table) (on me) (on c d e) (inside table1 table2 table) (between 1 2) (add 1 2))~%")
(format t "Input: ~a~%List of Constants In Domain: ~a"
'(store-facts ((on a b) (on a b c) (on table) (on me) (on c d e) (inside table1 table2 table) (between 1 2) (add 1 2)) ht)
(store-facts '((on a b) (on a b c) (on table) (on me) (on c d e) (inside table1 table2 table) (between 1 2) (add 1 2)) ht))

;; To check all the keys held by 'ht' currently, uncomment the following:
;;(format t "~%~%HT holds the following KEYS currently: ~a~%"
;;(loop for key being the hash-keys of ht collect key))

;; Using the gethash function now to retrieve the values
;; corresponding the keys
(format t "~%~%NOW TESTING WITH THE GETHASH FUNCTION:~%")
(format t "~%Input: (gethash '~a ht)~%" 'on)
(format t "Expected: (ON A B ON A B C ON TABLE ON ME ON C D E)~%Actual: ~a~%"
(gethash 'on ht))

(format t "~%Input: (gethash '~a ht)~%" '(on a b))
(format t "Expected: (ON A B)~%Actual: ~a~%"
(gethash '(on a b) ht))

(format t "~%Input: (gethash '~a ht)~%" '(on a - -))
(format t "Expected: (ON A B C)~%Actual: ~a~%"
(gethash '(on a - -) ht))

(format t "~%Input: (gethash '~a ht)~%" '(on - - c))
(format t "Expected: (ON A B C)~%Actual: ~a~%"
(gethash '(on - - c) ht))

;; Testing for Keys which will do not exist

(format t "~%Input: (gethash '~a ht)~%" '(on a - c))
(format t "Expected: NIL~%Actual: ~a~%"
(gethash '(on a - c) ht))

(format t "~%Input: (gethash '~a ht)~%" '(on a b c -))
(format t "Expected: NIL~%Actual: ~a~%~%~%"
(gethash '(on a b c -) ht))

(terpri)
(terpri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 4. Function to project an n-dimensional relation (n>1) onto some of its
;;;; dimensions, producing a k-dimensional relations (0<k<n).

(format t "TESTING FUNCTION 'PROJECT-RELATION'~%~%")

(format t "Input: ~a~%Expected: ((C A) (E B) (F D))~%Actual: ~a~%~%"
'(project-relation '((a b c)(b d e)(a e c)(b b e)(d e f)) '(x y z) '(z x))
(project-relation '((a b c)(b d e)(a e c)(b b e)(d e f)) '(x y z) '(z x)))

(format t "Input: ~a~%Expected: ((A B) (B D) (A E) (B B) (D E))~%Actual: ~a~%~%"
'(project-relation '((a b c)(b d e)(a e c)(b b e)(d e f)) '(x y z) '(x y))
(project-relation '((a b c)(b d e)(a e c)(b b e)(d e f)) '(x y z) '(x y)))

(format t "Input: ~a~%Expected: ((T T R Q) (T T P Q))~%Actual: ~a~%~%"
'(project-relation '((p q r s t)(r q p s t)) '(a b c d e) '(e e c b))
(project-relation '((p q r s t)(r q p s t)) '(a b c d e) '(e e c b)))

(format t "Input: ~a~%Expected: ((NIL 3 2)(5 2 1)(2 3 NIL))~%Actual: ~a~%~%"
'(project-relation '((1 2 3 nil)(3 1 2 5)(nil nil 3 2)) '(w x y z) '(z y x))
(project-relation '((1 2 3 nil)(3 1 2 5)(nil nil 3 2)) '(w x y z) '(z y x)))

;; WHEN THE LENGTH OF ALL-VARS > N (IT CONSIDERS THE OTHERS TO BE NIL)

(format t "Input: ~a~%Expected: ((NIL A) (NIL B) (NIL D))~%Actual: ~a~%~%"
'(project-relation '((a b)(b d)(a e)(b b)(d e)) '(x y z) '(z x))
(project-relation '((a b)(b d)(a e)(b b)(d e)) '(x y z) '(z x)))

(format t "Input: ~a~%Expected: ((CAT APPLE DOG) (APPLE CAT BANANA)~%Actual: ~a~%~%"
'(project-relation '((Apple Banana Cat Dog)(Cat Dog Apple Banana)) '(a b c d) '(c a d))
(project-relation '((Apple Banana Cat Dog)(Cat Dog Apple Banana)) '(a b c d) '(c a d)))

(format t "Input: ~a~%Expected: ((2 3 5 1 2 3))~%Actual: ~a~%~%"
'(project-relation '((4 3 5 1 2)) '(v w x y z) '(z w x y z w))
(project-relation '((4 3 5 1 2)) '(v w x y z) '(z w x y z w)))

(format t "Input: ~a~%Expected: ((O N M N O) (M N O N M) (M O N O M))~%Actual: ~a~%~%"
'(project-relation '((m n o)(o n m)(m n o)(n o m)) '(1 2 3) '(3 2 1 2 3))
(project-relation '((m n o)(o n m)(m n o)(n o m)) '(1 2 3) '(3 2 1 2 3)))

(format t "Input: ~a~%Expected: ((3 2) (4 1) (1 1) (2 3))~%Actual: ~a~%~%"
'(project-relation '((2 3 1 4)(1 4 2 3)(1 1 1 2)(3 2 1 3)) '(1 2 3 4) '(2 1))
(project-relation '((2 3 1 4)(1 4 2 3)(1 1 1 2)(3 2 1 3)) '(1 2 3 4) '(2 1)))

(format t "Input: ~a~%Expected: ((A4 A4 A1 A1) (B4 B4 B1 B1))~%Actual: ~a~%~%"
'(project-relation '((a1 a2 a3 a4)(b1 b2 b3 b4)) '(p q r s) '(s s p p))
(project-relation '((a1 a2 a3 a4)(b1 b2 b3 b4)) '(p q r s) '(s s p p)))

(terpri)
(terpri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 5. Function to return a list of tuples where the element in constr
;;;; exists at position posn.

(format t "TESTING FUNCTION 'CONSTRAIN-RELATION'~%~%")

(format t "Input: ~a~%Expected: ((A B) (B D))~%Actual: ~a~%~%"
'(constrain-relation '((c a)(a b)(b d)) '((a) (b) (e) (f)) 1)
(constrain-relation '((c a)(a b)(b d)) '((a) (b) (e) (f)) 1))

(format t "Input: ~a~%Expected: ((C A) (A B))~%Actual: ~a~%~%"
'(constrain-relation '((c a)(a b)(b d)) '((a) (b) (e) (f)) 2)
(constrain-relation '((c a)(a b)(b d)) '((a) (b) (e) (f)) 2))

(format t "Input: ~a~%Expected: ((CD AB) (EF AB))~%Actual: ~a~%~%"
'(constrain-relation '((ab cd)(cd ab)(ef ab)) '((ab) (ef) (xy) (pq)) 2)
(constrain-relation '((ab cd)(cd ab)(ef ab)) '((ab) (ef) (xy) (pq)) 2))

(format t "Input: ~a~%Expected: ((D A B C)(A B C D)(B C A D))~%Actual: ~a~%~%"
'(constrain-relation '((a b c d)(b c a d)(d a b c)) '((a) (b) (c) (d)) 4)
(constrain-relation '((a b c d)(b c a d)(d a b c)) '((a) (b) (c) (d)) 4))

(format t "Input: ~a~%Expected: ((NIL 1 2) (2 1 3))~%Actual: ~a~%~%"
'(constrain-relation '((nil 1 2)(2 1 nil)(2 1 3)) '((1) (2) (3)) 3)
(constrain-relation '((nil 1 2)(2 1 nil)(2 1 3)) '((1) (2) (3)) 3))

;;LONGER EXAMPLES

(format t "Input: ~a~%Expected: ((1 2 3 4) (1 2 3 4) (1 2 3 4) (4 1 2 3) (4 1 2 3) (4 1 2 3))~%Actual: ~a~%~%"
'(constrain-relation '((1 2 3 4)(2 3 4 1)(3 4 1 2)(4 1 2 3)(1 2 3 4)(2 3 4 1)(3 4 1 2)(4 1 2 3)(1 2 3 4)(2 3 4 1)(3 4 1 2)(4 1 2 3)) '((5) (2) (1)) 2)
(constrain-relation '((1 2 3 4)(2 3 4 1)(3 4 1 2)(4 1 2 3)(1 2 3 4)(2 3 4 1)(3 4 1 2)(4 1 2 3)(1 2 3 4)(2 3 4 1)(3 4 1 2)(4 1 2 3)) '((5) (2) (1)) 2))

(format t "Input: ~a~%Expected: ((D E A B) (E F A B) (C B A F) (F A B C) (F A B A))~%Actual: ~a~%~%"
'(constrain-relation '((a b c d)(d e a b)(f a b c)(d d c a)(a b d d)(a b c b)(d c c a)(e f a b)(b a c d)(f a b a)(c b a f)(e e f b)) '((e) (a) (b)) 3)
(constrain-relation '((a b c d)(d e a b)(f a b c)(d d c a)(a b d d)(a b c b)(d c c a)(e f a b)(b a c d)(f a b a)(c b a f)(e e f b)) '((e) (a) (b)) 3))

;;INPUT WITH NIL ARGUMENTS IN CONSTR

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(constrain-relation '((1 2)(2 3)(3 4)) '() 3)
(constrain-relation '((1 2)(2 3)(3 4)) '() 3))

;;INPUT WITH NIL ARGUMENTS IN RELN

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(constrain-relation '() '((1) (2) (3)) 1)
(constrain-relation '() '((1) (2) (3)) 1))

;;TEST CASES WHICH WILL RETURN NIL

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(constrain-relation '((c a)(a b)(b d)) '((a) (b) (e) (f)) 3)
(constrain-relation '((c a)(a b)(b d)) '((a) (b) (e) (f)) 3))

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%"
'(constrain-relation '((c a)(a b)(b d)) '((a) (b) (e) (f)) 4)
(constrain-relation '((c a)(a b)(b d)) '((a) (b) (e) (f)) 4))

)

 ;;Calling the test function to run all the required tests
(test)