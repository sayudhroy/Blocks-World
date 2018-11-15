;;;;Lisp Problem Set 5 - Test Cases
;;;;Sayudh Roy

(load "sroy8_src.lisp")
;; Loading the source code file

(defun test()

(defparameter *kb-ht* (make-hash-table :test #'equal))
;; Declaring the global hash-table 'knowledge base'

(defparameter depth (make-hash-table :test #'equal))
;; Declaring the global hash-table to store the depth for each block

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
	
(format t "~%~%Testing Function REMOVE-FACT:~%~%")
;; The function returns T if the fact existed in the hash table and has
;; been removed from it, else returns NIL if it didn't exist in the ht

;; Testing TRUE OUTPUT Inputs

(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(remove-fact '(on a b) *kb-ht*)
			(remove-fact '(on a b) *kb-ht*))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(remove-fact '(clear a) *kb-ht*)
			(remove-fact '(clear a) *kb-ht*))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(remove-fact '(green c) *kb-ht*)
			(remove-fact '(green c) *kb-ht*))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(remove-fact '(on f g) *kb-ht*)
			(remove-fact '(on f g) *kb-ht*))
(format t	"Input: ~a~%Expected: T~%Actual: ~a~%~%" 
			'(remove-fact '(on d table) *kb-ht*)
			(remove-fact '(on d table) *kb-ht*))
			
;; Testing NIL OUTPUT Inputs

(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(remove-fact '(on x y) *kb-ht*)
			(remove-fact '(on x y) *kb-ht*))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(remove-fact '(clear g) *kb-ht*)
			(remove-fact '(clear g) *kb-ht*))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(remove-fact '(blue c) *kb-ht*)
			(remove-fact '(blue c) *kb-ht*))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(remove-fact '(on i h) *kb-ht*)
			(remove-fact '(on i h) *kb-ht*))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(remove-fact '(on b table) *kb-ht*)
			(remove-fact '(on b table) *kb-ht*))
			
(format t "~%Updated Knowledge Base~%(KEY: VALUE)~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))

(format t "~%~%Resetting the Knowledge-Base")
(defparameter *kb-ht* (make-hash-table :test #'equal))
(defparameter depth (make-hash-table :test #'equal))
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
(mark-depth 'a 0)
(mark-depth 'p 0)
(mark-depth 'f 0)

(format t "~%~%Testing Function CLEAR-OFF:~%~%")

;; Testing TRUE OUTPUT Inputs
;; Returns the list of the blocks which are being removed to clear it

(format t	"Input: ~a~%Expected: (A)~%Actual: ~a~%~%" 
			'(clear-off 'b)	(clear-off 'b))
(format t	"Input: ~a~%Expected: (G F)~%Actual: ~a~%~%" 
			'(clear-off 'h)	(clear-off 'h))
(format t	"Input: ~a~%Expected: (Q P)~%Actual: ~a~%~%" 
			'(clear-off 'r)	(clear-off 'r))

;; Testing NIL OUTPUT Inputs			

(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(clear-off 'a)	(clear-off 'a))
(format t	"Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
			'(clear-off 'f)	(clear-off 'f))
			
;; Testing INVALID Inputs

(format t "Input: ~a~%" '(clear-off 'table))
(format t "Expected: 'TABLE' is an invalid input to Clear-Off!")
(format t "~%Actual: ") (clear-off 'table)
(format t "~%~%")

(format t "Input: ~a~%" '(clear-off 's))
(format t "Expected: 'S' is an invalid input to Clear-Off!")
(format t "~%Actual: ") (clear-off 's)
(format t "~%~%")

(format t "~%~%Resetting the Knowledge-Base")
(defparameter *kb-ht* (make-hash-table :test #'equal))
(defparameter depth (make-hash-table :test #'equal))
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
(mark-depth 'a 0)
(mark-depth 'p 0)
(mark-depth 'f 0)

(format t "~%~%Testing Function ORDER-RELATIONS:~%~%")

;; Testing PROPER OUTPUT Inputs

(format t "Input: ~a~%"
'(order-relations 	'(and (on ?x ?y) (red ?x) (blue ?y) (block ?x)
					(block ?y) (on ?y table))))
(format t "Expected: ((ON ?Y TABLE) (ON ?X ?Y))")
(format t "~%Actual: ~a"
(order-relations 	'(and (on ?x ?y) (red ?x) (blue ?y) (block ?x)
					(block ?y) (on ?y table))))
(format t "~%~%")

(format t "Input: ~a~%"
'(order-relations 	'(and (on b2 ?y) (on ?y ?x) (small ?y) (big ?x)
(on ?x b1) (on b1 table))))
(format t "Expected: ((ON B1 TABLE) (ON ?X B1) (ON ?Y ?X) (ON B2 ?Y))")
(format t "~%Actual: ~a"
(order-relations 	'(and (on b2 ?y) (on ?y ?x) (small ?y) (big ?x)
(on ?x b1) (on b1 table))))
(format t "~%~%")

(format t "Input: ~a~%"
'(order-relations 	'(and (on b2 table) (on b1 b2) (small b1) (big ?x)
(on ?x b1) (on ?y ?x) (on ?z ?y))))
(format t
"Expected: ((ON B2 TABLE) (ON B1 B2) (ON ?X B1) (ON ?Y ?X) (ON ?Z ?Y))")
(format t "~%Actual: ~a"
(order-relations 	'(and (on b2 table) (on b1 b2) (small b1) (big ?x)
(on ?x b1) (on ?y ?x) (on ?z ?y))))
(format t "~%~%")


(format t "Input: ~a~%"
'(order-relations 	'(and (on ?x table) (small b1) (big ?x)
					(on b1 ?x))))
(format t
"Expected: ((ON ?X TABLE) (ON B1 ?X))")
(format t "~%Actual: ~a"
(order-relations 	'(and (on ?x table) (small b1) (big ?x)
					(on b1 ?x))))
(format t "~%~%")

(format t "Input: ~a~%" '(order-relations 	'(on ?x table)))
(format t "Expected: ((ON ?X TABLE))")
(format t "~%Actual: ~a" (order-relations 	'(on ?x table)))
(format t "~%~%")

;; Testing INPUTS (without (on - table) fact) 

(format t "Input: ~a~%"
'(order-relations 	'(and (on b2 ?y) (on ?y ?x) (small ?y) (big ?x)
(on ?x b1))))
(format t "Expected: NIL")
(format t "~%Actual: ~a"
(order-relations 	'(and (on b2 ?y) (on ?y ?x) (small ?y) (big ?x)
(on ?x b1))))
(format t "~%~%")

(format t "Input: ~a~%"
'(order-relations 	'(and (on b1 b2) (small b1) (big ?x)
(on ?x b1) (on ?y ?x) (on ?z ?y))))
(format t
"Expected: NIL")
(format t "~%Actual: ~a"
(order-relations 	'(and (on b1 b2) (small b1) (big ?x)
(on ?x b1) (on ?y ?x) (on ?z ?y))))
(format t "~%~%")

(format t "Input: ~a~%"
'(order-relations 	'(and (on ?x table) (on ?y ?x) (on ?z ?y)
(on q w))))
(format t
"Expected: NIL")
(format t "~%Actual: ~a"
(order-relations 	'(and (on ?x table) (on ?y ?x) (on ?z ?y)
(on q w))))
(format t "~%~%")

;; Testing BAD INPUTS 

(format t "Input: ~a~%"
'(order-relations 	'((on b2 ?y) (on ?y ?x) (small ?y) (big ?x)
(on ?x b1))))
(format t "Expected: Invalid Input in Order-Relations.")
(format t "~%Actual: ")
(order-relations 	'((on b2 ?y) (on ?y ?x) (small ?y) (big ?x)
(on ?x b1)))
(format t "~%~%")

(format t "Input: ~a~%"
'(order-relations 	'(not (on ?x table) (small b1) (big ?x)
					(on b1 ?x))))
(format t "Expected: Invalid Input in Order-Relations.")
(format t "~%Actual: ")
(order-relations 	'(not (on ?x table) (small b1) (big ?x)
					(on b1 ?x)))
(format t "~%~%")

(format t "~%~%Testing Function ISOLATE-OBJ-DESCRIPTION:~%~%")

;; Testing PROPER OUTPUT Inputs

(format t "Input: ~a~%"
'(isolate-obj-description '?x '(and (on ?x ?y) (red ?x) (blue ?y)
							(block ?x) (block ?y) (on ?y table))))
(format t "Expected: ((RED ?X) (BLOCK ?X))")
(format t "~%Actual: ~a"
(isolate-obj-description '?x '(and (on ?x ?y) (red ?x) (blue ?y)
							(block ?x) (block ?y) (on ?y table))))
(format t "~%~%")

(format t "Input: ~a~%"
'(isolate-obj-description '?y '(and (on b2 ?y) (on ?y ?x) (small ?y)
							(big ?x) (on ?x b1) (on b1 table))))
(format t "Expected: ((SMALL ?Y))")
(format t "~%Actual: ~a"
(isolate-obj-description '?y '(and (on b2 ?y) (on ?y ?x) (small ?y)
							(big ?x) (on ?x b1) (on b1 table))))
(format t "~%~%")

(format t "Input: ~a~%"
'(isolate-obj-description '?x '(and (on b2 table) (on b1 b2) (small b1) 						(big ?x) (on ?x b1) (on ?y ?x) (on ?z ?y))))
(format t
"Expected: ((BIG ?X))")
(format t "~%Actual: ~a"
(isolate-obj-description '?x '(and (on b2 table) (on b1 b2) (small b1) 							(big ?x) (on ?x b1) (on ?y ?x) (on ?z ?y))))
(format t "~%~%")


(format t "Input: ~a~%"
'(isolate-obj-description '?x '(and (on ?x table) (small b1) (big ?x)
					(on b1 ?x))))
(format t "Expected: ((BIG ?X))")
(format t "~%Actual: ~a"
(isolate-obj-description '?x '(and (on ?x table) (small b1) (big ?x)
					(on b1 ?x))))
(format t "~%~%")

(format t "Input: ~a~%"
'(isolate-obj-description '?y '(and (on b2 ?y) (on ?y ?x) (small ?y)
							(big ?x) (on ?x b1))))
(format t "Expected: ((SMALL ?Y))")
(format t "~%Actual: ~a"
(isolate-obj-description '?y '(and (on b2 ?y) (on ?y ?x) (small ?y)
							(big ?x) (on ?x b1))))
(format t "~%~%")

;; Testing PROPER (NIL) Output Inputs

(format t "Input: ~a~%" '(isolate-obj-description '?x '(on ?x table)))
(format t "Expected: NIL")
(format t "~%Actual: ~a" (isolate-obj-description '?x '(on ?x table)))
(format t "~%~%")

(format t "Input: ~a~%"
'(isolate-obj-description '?y '(and (on ?x table) (small b1) (big ?x)
					(on b1 ?x))))
(format t "Expected: NIL")
(format t "~%Actual: ~a"
(isolate-obj-description '?y '(and (on ?x table) (small b1) (big ?x)
					(on b1 ?x))))
(format t "~%~%")

;; Testing BAD INPUTS 

(format t "Input: ~a~%"
'(isolate-obj-description '?x '((on b2 ?y) (on ?y ?x) (small ?y)
							(big ?x) (on ?x b1))))
(format t
"Expected: Invalid Input in Function Isolate-Obj-Description.")
(format t "~%Actual: ")
(isolate-obj-description '?x '((on b2 ?y) (on ?y ?x) (small ?y)
							(big ?x) (on ?x b1)))
(format t "~%~%")

(format t "Input: ~a~%"
'(isolate-obj-description '?y '(not (on ?x table) (small b1) (big ?x)
					(on b1 ?x))))
(format t
"Expected: Invalid Input in Function Isolate-Obj-Description.")
(format t "~%Actual: ")
(isolate-obj-description '?y '(not (on ?x table) (small b1) (big ?x)
					(on b1 ?x)))
(format t "~%~%")

;; RESTORING FACTS IN HASH TABLE
(defparameter *kb-ht* (make-hash-table :test #'equal))
(defparameter depth (make-hash-table :test #'equal))
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
(mark-depth 'a 0)
(mark-depth 'p 0)
(mark-depth 'f 0)

(format t "~%~%Printing Knowledge Base Before Function Stack-up:~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))

(format t "~%~%Testing Function STACK-UP with following inputs:~%~%")	

(format t "~%~%1. Input: ~a~%"
	'(stack-up 'd 'table)) (stack-up 'd 'table)
(format t "~%~%2. Input: ~a~%" 
	'(stack-up 'p 'table)) (stack-up 'p 'table)
(format t "~%~%3. Input: ~a~%" '(stack-up 'h 'q)) (stack-up 'h 'q)
(format t "~%~%4. Input: ~a~%" '(stack-up 'j 'a)) (stack-up 'j 'a)
(format t "~%~%5. Input: ~a~%" '(stack-up 'c 'h)) (stack-up 'c 'h)
(format t "~%~%6. Input: ~a~%" '(stack-up 'b 'q)) (stack-up 'b 'q)

(format t "~%~%Knowledge Base After Function Stack-up:~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))
(format t "~%~%Depth Hash-Table After Funtion Stack-Up~%~%")
(loop for key being the hash-keys of depth do
	(format t "~a: ~a~%" key (gethash key depth)))
	
(format t "~%~%Testing Function ACHIEVE:~%~%")

(format t "~%~%Resetting & loading hash-tables with new facts:~%~%")
;; RESTORING FACTS IN HASH TABLE
(defparameter *kb-ht* (make-hash-table :test #'equal))
(defparameter depth (make-hash-table :test #'equal))
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
(mark-depth 'a 0)
(mark-depth 'p 0)
(mark-depth 'f 0)

(format t "~%~%New Knowledge Base Before Function Achieve #1:~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))

(format t "1. Input: ~a~%"
'(achieve 	'(and (on ?x ?y) (red ?x) (blue ?y) (block ?x)
					(block ?y) (on ?y table))))
(achieve 	'(and (on ?x ?y) (red ?x) (blue ?y) (block ?x)
					(block ?y) (on ?y table)))
(format t "~%~%")

(format t "~%~%Knowledge Base After Function Achieve #1:~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))
(format t "~%Some of the relations are a result of applying the
			function name-and-store relation in the process.~%~%")
(format t "~%~%Depth Hash-Table After Funtion Achieve #1:~%~%")
(loop for key being the hash-keys of depth do
	(format t "~a: ~a~%" key (gethash key depth)))
	
;; RESTORING FACTS IN HASH TABLE
(format t "~%~%Resetting & loading hash-tables with new facts:~%~%")
(defparameter *kb-ht* (make-hash-table :test #'equal))
(defparameter depth (make-hash-table :test #'equal))
(store-facts  '(;; The List of Blocks
				(block c)(block d)
				(block p)(block q)(block r)(block s)
				;; Listing CLEAR Blocks
				(clear c)(clear s)(clear table)
				;; The ON Relations of the Blocks
				(on c d)(on d table)
				(on s r)(on r q)(on q p)(on p table))
				*kb-ht*)
(mark-depth 'c 0)
(mark-depth 's 0)
(format t "~%~%New Knowledge Base Before Function Achieve #2:~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))

(format t "~%~%2. Input: ~a~%"
'(achieve '(and (on ?x table) (on ?y ?x) (on q ?y))))
(achieve '(and (on ?x table) (on ?y ?x) (on q ?y)))
(format t "~%~%")

(format t "~%~%Knowledge Base After Function Achieve #2:~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))
(format t "~%Some of the relations are a result of applying the
			function name-and-store relation in the process.~%~%")
(format t "~%~%Depth Hash-Table After Funtion Achieve #2:~%~%")
(loop for key being the hash-keys of depth do
	(format t "~a: ~a~%" key (gethash key depth)))
	
;; RESTORING FACTS IN HASH TABLE
(defparameter *kb-ht* (make-hash-table :test #'equal))
(defparameter depth (make-hash-table :test #'equal))
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
(mark-depth 'a 0)
(mark-depth 'p 0)
(mark-depth 'f 0)

(format t "~%~%New Knowledge Base Before Function Achieve #3:~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))
(format t "3. Input: ~a~%"
'(achieve 	'(and (on ?x ?y) (green ?x) (red ?y) (block ?x)
					(block ?y) (on ?y table))))
(achieve 	'(and (on ?x ?y) (green ?x) (red ?y) (block ?x)
					(block ?y) (on ?y table)))
(format t "~%~%")

(format t "~%~%Knowledge Base After Function Achieve #3: ~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))
(format t "~%Some of the relations are a result of applying the
			function name-and-store relation in the process.~%~%")
(format t "~%~%Depth Hash-Table After Funtion Achieve #3: ~%~%")
(loop for key being the hash-keys of depth do
	(format t "~a: ~a~%" key (gethash key depth)))

;; RESTORING FACTS IN HASH TABLE
(format t "~%~%Resetting & loading hash-tables with new facts:~%~%")
(defparameter *kb-ht* (make-hash-table :test #'equal))
(defparameter depth (make-hash-table :test #'equal))
(store-facts  '(;; The List of Blocks
				(block c)(block d) (red c) (green d)
				(block r)(block s) (blue r) (red s)
				;; Listing CLEAR Blocks
				(clear c)(clear s)(clear table)
				;; The ON Relations of the Blocks
				(on c d)(on d table)
				(on s r)(on r table))
				*kb-ht*)
(mark-depth 'c 0)
(mark-depth 's 0)
(format t "~%~%New Knowledge Base Before Function Achieve #4:~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))

(format t "~%~%4. Input: ~a~%"
'(achieve '(and (on ?x table) (on ?y ?x) (on r ?y) (green ?y))))
(achieve '(and (on ?x table) (on ?y ?x) (on r ?y) (green ?y)))
(format t "~%~%")	

;; RESTORING FACTS IN HASH TABLE
(defparameter *kb-ht* (make-hash-table :test #'equal))
(defparameter depth (make-hash-table :test #'equal))
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
(mark-depth 'a 0)
(mark-depth 'p 0)
(mark-depth 'f 0)
 
(format t "~%~%New Knowledge Base Before Function Achieve #5:~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))

(format t "5. Input: ~a~%"
'(achieve '(and (on ?x table) (red ?x) (on g ?x) (on ?y g) (blue ?y)
(on ?z ?y))))
(achieve '(and (on ?x table) (red ?x) (on g ?x) (on ?y g) (blue ?y)
(on ?z ?y)))
(format t "~%~%")

(format t "~%~%Knowledge Base After Function Achieve #5:~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))
(format t "~%Some of the relations are a result of applying the
			function name-and-store relation in the process.~%~%")
(format t "~%~%Depth Hash-Table After Funtion Achieve #5:~%~%")
(loop for key being the hash-keys of depth do
	(format t "~a: ~a~%" key (gethash key depth)))
	

)

 ;;Calling the test function to run all the required tests
(test)