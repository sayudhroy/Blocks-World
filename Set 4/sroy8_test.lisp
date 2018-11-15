;;;;Lisp Problem Set 3 - Test Cases
;;;;Sayudh Roy

(defun test()

;; Creating a knowledge-base in the global hash-table *kb-ht*
(store-facts  '((on b1 table1)
				(on table1 b1)
				(on b2 b1)
				(on b3 table1)
				(on b4 table2)
				(on b5 table2)
				(on b5 table3)
				(block b1)
				(block b2)
				(block b3)
				(block b4)
				(block b5)
				(block table1)
				(block table2)
				(block table3)
				(red b1)
				(clear b1)
				(blue b2)
				(clear b3)
				(clear b4)
				(red b5)
				(blue table1)
				(red table2)
				(clear table3))
				*kb-ht*)

(format t "~%~%Current Knowledge Base~%(KEY: VALUE)~%~%")
(loop for key being the hash-keys of *kb-ht* do
	(format t "~a: ~a~%" key (gethash key *kb-ht*)))
		

(format t "~%~%Testing function 'FIND-WHETHER':~%~%")		
		
;;Testing Normal Cases
		
(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(on b3 table1))
(find-whether '(on b3 table1)))

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(and (clear b3) (on b3 table1)))
(find-whether '(and (clear b3) (on b3 table1))))

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x table1) (red ?x) (clear ?x)))
(find-whether '(and (on ?x table1) (red ?x) (clear ?x))))

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(and (on b3 ?y) (block ?y) (not (red ?y))))
(find-whether '(and (on b3 ?y) (block ?y) (not (red ?y)))))

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x ?y) (on b3 table1) (block ?x) (red ?y)))
(find-whether '(and (on ?x ?y) (on b3 table1) (block ?x) (red ?y))))

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x table1) (red ?x)))
(find-whether '(and (on ?x table1) (red ?x))))

;;Testing Double Variable Cases

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(on ?x ?y))
(find-whether '(on ?x ?y)))

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x ?y) (on ?y b1)))
(find-whether '(and (on ?x ?y) (on ?y b1))))

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x ?y) (on ?y ?x)))
(find-whether '(and (on ?x ?y) (on ?y ?x))))

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x ?y) (red ?x) (clear ?y)))
(find-whether '(and (on ?x ?y) (red ?x) (clear ?y))))

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x ?y) (block ?x) (block ?y) (red ?y)))
(find-whether '(and (on ?x ?y) (block ?x) (block ?y) (red ?y))))

(format t "Input: ~a~%Expected: T~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x ?y) (block ?x) (block ?y) (red ?y)
(clear ?x)))
(find-whether '(and (on ?x ?y) (block ?x) (block ?y) (red ?y)
(clear ?x))))

;;Testing Nil Cases
		
(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x table1) (blue ?x) (red table2)))
(find-whether '(and (on ?x table1) (blue ?x) (red table2))))

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x table1) (blue ?x) (blue table2)))
(find-whether '(and (on ?x table1) (blue ?x) (blue table2))))

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
'(find-whether '(and (on ?x ?y) (block ?x) (block ?y) (red ?y)
(green ?x)))
(find-whether '(and (on ?x ?y) (block ?x) (block ?y) (red ?y)
(green ?x))))

;;Testing Bad Inputs

(format t "Input: ~a" '(find-whether '(and (on b3 table1))))
(format t "~%Expected: Invalid Input(s) Entered!~%Actual: ")
(find-whether '(and (on b3 table1)))
(format t "~%~%")

(format t "Input: ~a" '(find-whether '(and (block ?x)
(on ?x ?y ?z))))
(format t "~%Expected: Invalid Input(s) Entered!~%Actual: ")
(find-whether '(find-whether '(and (block ?x) (on ?x ?y ?z))))
(format t "~%~%")

(format t "Input: ~a" '(find-whether '(or (on b3 table1)
(block b3))))
(format t "~%Expected: Invalid Input(s) Entered!~%Actual: ")
(find-whether '(or (on b3 table1) (block b3)))
(format t "~%~%")

(format t "Input: ~a" '(find-whether 'block))
(format t "~%Expected: Invalid Input(s) Entered!~%Actual: ")
(find-whether 'block)
(format t "~%~%")

(format t "~%~%Testing function 'FIND-ALL-INSTANCES':~%~%")		
		
;;Testing Normal Cases
		
(format t "Input: ~a~%Expected: ((B1))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x) (and (on ?x table1) (red ?x)
(clear ?x))))
(find-all-instances '(:l (?x) (and (on ?x table1) (red ?x)
(clear ?x)))))

(format t "Input: ~a~%Expected: ((TABLE1))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?y) (and (on b3 ?y) (block ?y)
(not (red ?y)))))
(find-all-instances '(:l (?y) (and (on b3 ?y) (block ?y)
(not (red ?y))))))

(format t "Input: ~a~%Expected: ((TABLE1) (B2) (B4) (B5))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x) (and (on ?x ?y) (on b3 table1)
(block ?x) (red ?y))))
(find-all-instances '(:l (?x) (and (on ?x ?y) (on b3 table1)
(block ?x) (red ?y)))))

(format t "Input: ~a~%Expected: ((B1))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x) (and (on ?x table1) (red ?x))))
(find-all-instances '(:l (?x) (and (on ?x table1) (red ?x)))))

;;Testing Double Variable Cases

(format t "Input: ~a~%Expected: ((B1 TABLE1) (TABLE1 B1) (B2 B1) (B3 TABLE1) (B4 TABLE2) (B5 TABLE2) (B5 TABLE3))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x ?y) (on ?x ?y)))
(find-all-instances '(:l (?x ?y) (on ?x ?y))))

(format t "Input: ~a~%Expected: ((TABLE1 B1) (B2 B1))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x ?y) (and (on ?x ?y) (on ?y b1))))
(find-all-instances '(:l (?x ?y) (and (on ?x ?y) (on ?y b1)))))

(format t "Input: ~a~%Expected: ((B1 TABLE1) (TABLE1 B1))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x ?y) (and (on ?x ?y) (on ?y ?x))))
(find-all-instances '(:l (?x ?y) (and (on ?x ?y) (on ?y ?x)))))

(format t "Input: ~a~%Expected: ((B5 TABLE3))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x ?y) (and (on ?x ?y) (red ?x)
(clear ?y))))
(find-all-instances '(:l (?x ?y) (and (on ?x ?y) (red ?x)
(clear ?y)))))

(format t "Input: ~a~%Expected: ((TABLE1 B1) (B2 B1) (B4 TABLE2) (B5 TABLE2))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x ?y) (and (on ?x ?y) (block ?x)
(block ?y) (red ?y))))
(find-all-instances '(:l (?x ?y) (and (on ?x ?y) (block ?x)
(block ?y) (red ?y)))))

(format t "Input: ~a~%Expected: ((B4 TABLE2))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x ?y) (and (on ?x ?y) (block ?x)
(block ?y) (red ?y) (clear ?x))))
(find-all-instances '(:l (?x ?y) (and (on ?x ?y) (block ?x)
(block ?y) (red ?y) (clear ?x)))))

(format t "Input: ~a~%Expected: ((TABLE1) (B2))~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x) (and (on ?x ?y) (block ?x)
(block ?y) (red ?y) (blue ?x))))
(find-all-instances '(:l (?x) (and (on ?x ?y) (block ?x)
(block ?y) (red ?y) (blue ?x)))))

;;Testing Nil Cases
		
(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x) (and (on ?x table1) (blue ?x)
(red table2))))
(find-all-instances '(:l (?x) (and (on ?x table1) (blue ?x)
(red table2)))))

(format t "Input: ~a~%Expected: NIL~%Actual: ~a~%~%" 
'(find-all-instances '(:l (?x) (and (on ?x table1) (blue ?x)
(blue table2))))
(find-all-instances '(:l (?x) (and (on ?x table1) (blue ?x)
(blue table2)))))

;;Testing Bad Inputs
		
(format t "Input: ~a" '(find-all-instances '(:l (?x)
(and (on b1 table1) (blue b1)))))
(format t "~%Expected: None of the predications contain any lambda variable!")
(format t "~%Actual: ")
(find-all-instances '(:l (?x) (and (on b1 table1) (blue b1))))
(format t "~%~%")

(format t "Input: ~a" '(find-all-instances '(:s (?x)
(and (on ?x table1) (blue b1)))))
(format t "~%Expected: Invalid Input(s) Entered!")
(format t "~%Actual: ")
(find-all-instances '(:s (?x) (and (on ?x table1) (blue b1))))
(format t "~%~%")

(format t "Input: ~a" '(find-all-instances '(:l ?x (and
(on ?x table1) (blue b1)))))
(format t "~%Expected: Invalid Input(s) Entered!")
(format t "~%Actual: ")
(find-all-instances '(find-all-instances '(:l ?x (and
(on ?x table1) (blue b1)))))
(format t "~%~%")
)

 ;;Calling the test function to run all the required tests
(test)