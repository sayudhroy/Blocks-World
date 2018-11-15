;;;;Lisp Problem Set 4 - Test Cases
;;;;Sayudh Roy

(defun test()

;; Creating a knowledge-base in the global hash-table *kb-ht*
(store-facts  '(;; The List of Blocks
				(block a)(block b)(block c)(block d)
				(block p)(block q)(block r)
				(block f)(block g)(block h)(block i)(block j)
				;; The Colours of the Blocks
				(red a)(blue b)(green c)(red d)
				(blue p)(red q)(green r)
				(blue f)(red g)(green h)(red i)(green j)
				;; Listing CLEAR Blocks
				(clear a)(clear p)(clear f)(clear table)
				;; The ON Relations of the Blocks
				(on a b)(on b c)(on c d)(on d table)
				(on p q)(on q r)(on r table)
				(on f g)(on g h)(on h i)(on i j)(on j table))
				*kb-ht*)

(format t "~%~%Current Knowledge Base~%(KEY: VALUE)~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))

(format t "~%~%Running the Function MARK-DEPTH on CLEAR BLOCKS:~%~%")
(format t "~a~%~a~%~a~%" '(mark-depth 'a 0) '(mark-depth 'p 0) 
						 '(mark-depth 'f 0))
(mark-depth 'a 0)
(mark-depth 'p 0)
(mark-depth 'f 0)
	
(format t "~%~%Current Depth Hash-Table~%(KEY: VALUE)~%~%")
(loop for key being the hash-keys of depth do
	(format t "~a: ~a~%" key (gethash key depth)))

(format t "~%~%Testing Function SAME-STACK:~%~%")

;; Testing Normal (TRUE OUTPUT) Inputs

(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'a 'b) (same-stack 'a 'b))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'a 'a) (same-stack 'a 'a))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'b 'c) (same-stack 'b 'c))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'a 'd) (same-stack 'a 'd))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'p 'q) (same-stack 'p 'q))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'p 'r) (same-stack 'p 'r))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'f 'i) (same-stack 'f 'i))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'h 'h) (same-stack 'h 'h))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'g 'f) (same-stack 'g 'f))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'd 'a) (same-stack 'd 'a))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(same-stack 'c 'b) (same-stack 'c 'b))
			
;; Testing Normal (NIL OUTPUT) Inputs

(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(same-stack 'a 'p) (same-stack 'a 'p))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(same-stack 'b 'h) (same-stack 'b 'h))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(same-stack 'q 'c) (same-stack 'q 'c))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(same-stack 'f 'd) (same-stack 'f 'd))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(same-stack 'h 'q) (same-stack 'h 'q))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(same-stack 'c 'r) (same-stack 'c 'r))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(same-stack 'a 'i) (same-stack 'a 'i))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(same-stack 'h 'd) (same-stack 'h 'd))

;; Testing Inputs with TABLE as an Input

(format t "Input: ~a~%" '(same-stack 'a 'table))
(format t "Expected: Input(s) do not have depth numbers. Invalid.")
(format t "~%Actual: ")(same-stack 'a 'table)
(format t "~%~%")

(format t "Input: ~a~%" '(same-stack 'table 'j))
(format t "Expected: Input(s) do not have depth numbers. Invalid.")
(format t "~%Actual: ")(same-stack 'table 'j)
(format t "~%~%")

(format t "Input: ~a~%" '(same-stack 'q 'table))
(format t "Expected: Input(s) do not have depth numbers. Invalid.")
(format t "~%Actual: ")(same-stack 'q 'table)
(format t "~%~%")

;; Testing Other Bad Inputs

(format t "Input: ~a~%" '(same-stack 'csc '444))
(format t "Expected: Input(s) do not have depth numbers. Invalid.")
(format t "~%Actual: ")(same-stack 'csc '444)
(format t "~%~%")

(format t "Input: ~a~%" '(same-stack 'lisp '4))
(format t "Expected: Input(s) do not have depth numbers. Invalid.")
(format t "~%Actual: ")(same-stack 'lisp '4)
(format t "~%~%")

(format t "Input: ~a~%" '(same-stack 'lfai 'c))
(format t "Expected: Input(s) do not have depth numbers. Invalid.")
(format t "~%Actual: ")(same-stack 'lfai 'c)
(format t "~%~%")

(format t "Input: ~a~%" '(same-stack 'a 'sroy8))
(format t "Expected: Input(s) do not have depth numbers. Invalid.")
(format t "~%Actual: ")(same-stack 'a 'sroy8)
(format t "~%~%")

(format t "~%~%Testing Function COST:~%~%")

;; Testing Normal Inputs

(format t	"Input: ~a~%Expected: 0~%Actual: ~a~%~%" 
			'(cost 'a 'b) (cost 'a 'b))
(format t	"Input: ~a~%Expected: 1~%Actual: ~a~%~%" 
			'(cost 'a 'table) (cost 'a 'table))
(format t	"Input: ~a~%Expected: 3~%Actual: ~a~%~%" 
			'(cost 'a 'c) (cost 'a 'c))
(format t	"Input: ~a~%Expected: 4~%Actual: ~a~%~%" 
			'(cost 'b 'd) (cost 'b 'd))
(format t	"Input: ~a~%Expected: 3~%Actual: ~a~%~%" 
			'(cost 'p 'r) (cost 'p 'r))
(format t	"Input: ~a~%Expected: 1~%Actual: ~a~%~%" 
			'(cost 'a 'p) (cost 'a 'p))
(format t	"Input: ~a~%Expected: 4~%Actual: ~a~%~%" 
			'(cost 'f 'd) (cost 'f 'd))
(format t	"Input: ~a~%Expected: 3~%Actual: ~a~%~%" 
			'(cost 'h 'table) (cost 'h 'table))
(format t	"Input: ~a~%Expected: 0~%Actual: ~a~%~%" 
			'(cost 'j 'table) (cost 'j 'table))
(format t	"Input: ~a~%Expected: 2~%Actual: ~a~%~%" 
			'(cost 'p 'g) (cost 'p 'g))
(format t	"Input: ~a~%Expected: 3~%Actual: ~a~%~%" 
			'(cost 'c 'table) (cost 'c 'table))
(format t	"Input: ~a~%Expected: 3~%Actual: ~a~%~%" 
			'(cost 'f 'r) (cost 'f 'r))
(format t	"Input: ~a~%Expected: 0~%Actual: ~a~%~%" 
			'(cost 'd 'table) (cost 'd 'table))
(format t	"Input: ~a~%Expected: 1~%Actual: ~a~%~%" 
			'(cost 'f 'a) (cost 'f 'a))
(format t	"Input: ~a~%Expected: 2~%Actual: ~a~%~%" 
			'(cost 'b 'a) (cost 'b 'a))
(format t	"Input: ~a~%Expected: 5~%Actual: ~a~%~%" 
			'(cost 'j 'g) (cost 'j 'g))
			
;; Testing With Same Blocks

(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(cost 'a 'a) (cost 'a 'a))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(cost 'h 'h) (cost 'h 'h))

;; Testing Bad Inputs

(format t "Input: ~a~%Expected: " '(cost 'table 'a))
(format t "Invalid Input. Has to be block-block / block-table.")
(format t "~%Actual: ")(cost 'table 'a)(format t "~%~%")

(format t "Input: ~a~%Expected: " '(cost 'x 'y))
(format t "Invalid Input. Has to be block-block / block-table.")
(format t "~%Actual: ")(cost 'x 'y)(format t "~%~%")

(format t "Input: ~a~%Expected: " '(cost '1 '2))
(format t "Invalid Input. Has to be block-block / block-table.")
(format t "~%Actual: ")(cost '1 '2)(format t "~%~%")

(format t "Input: ~a~%Expected: " '(cost 'csc '444))
(format t "Invalid Input. Has to be block-block / block-table.")
(format t "~%Actual: ")(cost 'csc '444)(format t "~%~%")

(format t "Input: ~a~%Expected: " '(cost 'a 'm))
(format t "Invalid Input. Has to be block-block / block-table.")
(format t "~%Actual: ")(cost 'a 'm)(format t "~%~%")

(format t "~%~%Testing Function STACKING-CANDIDATES:~%~%")

;; Testing Normal Inputs

(format t "Input: ~a~%"
		'(stacking-candidates '(a b c) '(p q)))
(format t "Expected: ((A P 1) (A Q 2) (B P 2) (B Q 3) (C P 3) (C Q 4))")
(format t "~%Actual: ~a~%~%"
		(stacking-candidates '(a b c) '(p q)))

(format t "Input: ~a~%"
		'(stacking-candidates
		'(:l (?x) (and (block ?x) (clear ?x)))
		'table))
(format t "Expected: ((A TABLE 1) (P TABLE 1) (F TABLE 1))")
(format t "~%Actual: ~a~%~%"
		(stacking-candidates
		'(:l (?x) (and (block ?x) (clear ?x)))
		'table))

(format t "Input: ~a~%"
		'(stacking-candidates
		'(:l (?x) (and (block ?x) (red ?x)))
		'(:l (?x) (and (block ?x) (blue ?x)))))
(format t "Expected: ((A B 0) (A P 1) (A F 1) (Q P 2) (Q F 2) (G P 2)
 (G F 2) (Q B 3) (G B 3) (D B 4) (D P 4) (D F 4) (I P 4) (I F 4) 
 (I B 5))")
(format t "~%Actual: ~a~%~%"
		(stacking-candidates
		'(:l (?x) (and (block ?x) (red ?x)))
		'(:l (?x) (and (block ?x) (blue ?x)))))

(format t "Input: ~a~%"
		'(stacking-candidates
		'(:l (?x) (and (block ?x) (red ?x) (clear ?x)))
        '(:l (?x) (and (block ?x) (blue ?x) (clear ?x)))))
(format t "Expected: ((A P 1) (A F 1))")
(format t "~%Actual: ~a~%~%"
		(stacking-candidates
		'(:l (?x) (and (block ?x) (red ?x) (clear ?x)))
        '(:l (?x) (and (block ?x) (blue ?x) (clear ?x)))))		


(format t "Input: ~a~%"
		'(stacking-candidates
		'(:l (?x) (and (block ?x) (red ?x)))
        '(:l (?x) (and (block ?x) (on ?x Table)))))
(format t "Expected: ((Q R 0) (I J 0) (A R 3) (A D 4) (G R 4) (A J 5)
 (Q D 5) (G D 5) (G J 5) (D R 6) (Q J 6) (I R 6) (I D 7) (D J 8))")
(format t "~%Actual: ~a~%~%"
		(stacking-candidates
		'(:l (?x)(and (block ?x) (red ?x)))
        '(:l (?x)(and (block ?x) (on ?x Table)))))
		
(format t "Input: ~a~%"
		'(stacking-candidates
		'(:l (?x) (and (block ?x) (red ?x)))
        '(:l (?x) (and (block ?x) (on ?x Table)))))
(format t "Expected: ((Q R 0) (I J 0) (A R 3) (A D 4) (G R 4) (A J 5)
 (Q D 5) (G D 5) (G J 5) (D R 6) (Q J 6) (I R 6) (I D 7) (D J 8))")
(format t "~%Actual: ~a~%~%"
		(stacking-candidates
		'(:l (?x)(and (block ?x) (red ?x)))
        '(:l (?x)(and (block ?x) (on ?x Table)))))

(format t "Input: ~a~%"
		'(stacking-candidates
		'(:l (?x) (and (block ?x) (red ?x)))
        '(:l (?x) (and (block ?x) (on ?x Table)))))
(format t "Expected: ((Q R 0) (I J 0) (A R 3) (A D 4) (G R 4) (A J 5)
 (Q D 5) (G D 5) (G J 5) (D R 6) (Q J 6) (I R 6) (I D 7) (D J 8))")
(format t "~%Actual: ~a~%~%"
		(stacking-candidates
		'(:l (?x)(and (block ?x) (red ?x)))
        '(:l (?x)(and (block ?x) (on ?x Table)))))
		
(format t "Input: ~a~%" '(stacking-candidates 'a 'a))
(format t "Expected: NIL")
(format t "~%Actual: ~a~%~%" (stacking-candidates 'a 'a))	

;; Testing Inputs With No Matches

(format t "Input: ~a~%" '(stacking-candidates '() '()))
(format t "Expected: Description(s) did not match any blocks.")
(format t "~%Actual: ") (stacking-candidates '() '()) (format t "~%~%")
		
;; Testing Invalid Inputs
		
(format t "Input: ~a~%" '(stacking-candidates 'table 'table))
(format t
"Expected: Invalid Input. Has to be block-block / block-table.")
(format t "~%Actual: ") (stacking-candidates 'table 'table)
(format t "~%~%")

(format t "Input: ~a~%" '(stacking-candidates 'csc444 'table))
(format t
"Expected: Invalid Input. Has to be block-block / block-table.")
(format t "~%Actual: ") (stacking-candidates 'csc444 'table)
(format t "~%~%")

)

 ;;Calling the test function to run all the required tests
(test)