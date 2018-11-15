;;;; Lisp Problem Set 4
;;;; Sayudh Roy

(defparameter *kb-ht* (make-hash-table :test #'equal))
;; Declaring the global hash-table 'knowledge base'

(defparameter depth (make-hash-table :test #'equal))
;; Declaring the global hash-table to store the depth for each block

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOLLOWING FUNCTIONS HAVE BEEN TAKEN FROM PREVIOUS ASSIGNMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helper Function: This 'update-table' function takes three
;;; arguments from the calling function namely:
;;; pred - the key to be added to the hash table
;;; val - the corresponding value for to store for the key
;;; ht - the hash table to be stored in
;;; The function stores the fact in the hash-table 'ht' under
;;; the key 'pred' and with the value 'val'. It checks if the key
;;; is already present in the hash-table 'ht'. If yes, it appends
;;; the new value to the key in the hash table, otherwise it adds
;;; a new key-value pair to the hash table.
;;; Although what this function does can be done simply in the
;;; calling function itself, since the calling function repeats
;;; the use of updating the table several times, I have used this
;;; helper function for clarity ONLY
(defun update-table(pred val ht)
	(if (gethash pred ht)
	(setf (gethash pred ht) (append (gethash pred ht) (list val)))
	(setf (gethash pred ht) (list val))))
	
;;;; STORE-FACT
;;;; Function to store a fact entered by the user in multiple
;;;; ways under different keys:
;;;; a. the predicate alone
;;;; b. the fact as a whole
;;;; c. the predicate together with each argument
;;;; This function uses the helper function 'update-table' to
;;;; store a fact in the hash table
;;;; NOTE: For this function to run a hash-table has to be created
;;;; which can be created using the following input:
;;;; (defparameter ht (make-hash-table :test #'equal))
(defun store-fact(fact ht)
	(cond ((gethash fact ht)
		;; Checks if the fact is already present in the hash-table,
		;; if yes, it shows an error and exits
		nil)
		(t (update-table (car fact) fact ht)
		;; Updates the hash-table with an entry with the predicate
		;; as the key for the entry and the fact as the value
		(if (cdr fact) (update-table fact fact ht))
		;; Checks if the fact has any arguments or not, if it does,
		;; it adds the entire fact as a key for the hash table also
		(if (> (length (cdr fact)) 1)
		;; Checks if there are more than one argument in the fact
			(progn	(loop for x from 0 to (- (length (cdr fact)) 1) do
			;; Traverses through the number of arguments
				(let ((c (copy-list (cdr fact))))
				;; Creates a local variable which temporarily stores
				;; the entire list of arguments from the fact
					(loop for y from 0 to (- (length c) 1) do
					;; Traverses through the list of arguments
						(if (not (equal x y))
							(setf (nth y c) '-)))
							;; Sets the element at the position where
							;; the x!=y to '-'
					(update-table (cons (car fact) c) fact ht))))))))
					;; Adds each of these updated arguments' lists
					;; appended to the predicate as an entry for the
					;; hash table

;;;; STORE-FACTS
;;;; Function that store facts in a hash-table
;;;; ht - the hash table to store the facts with their keys
;;;; facts - used in the 'store-facts' function to take all the
;;;; facts and pass it one by one to the 'store-fact' function to
;;;; add records to the hash table
(defun store-facts(facts ht)
	(mapcar (lambda (x) (store-fact x ht)) facts))
	
;;; Helper Function: This 'reorder' function takes two arguments
;;; from the calling function namely:
;;; reln - a list of elements (eg. (A B C))
;;; posns - a list of positions (eg. (3 1))
;;; The function creates a list out of 'reln' in such a way that
;;; the length of the list is that of the length of 'posns' (in
;;; this case 2) and the elements present in those positions are
;;; reordered into a list and sent returned to the calling func.
;;; For example: reln - (A B C), posns - (3 1), return - (C A)
(defun reorder(reln posns)
	(let ((lst))(loop for x in posns do
		(setf lst (append lst (list (nth x reln)))))lst))


;;; Helper Function: This getFact function takes two arguments
;;; from the calling function namely:
;;; reln - a list of elements
;;; posns - a list of positions
;;; The function creates a list out of reln such that the elements
;;; at the positions from posns are replaced by a '-' which can be
;;; referenced with the hash-table	
(defun getFact(reln posns)
	(let ((lst reln))(loop for x in posns do
		(setf (nth x lst) '-))lst))		

;;;; CONSTRAIN-PREDICATE
;;;; Function that selects a list of tuples from a relation
;;;; 'reln' under the constraint of another relation 'constraint'.
;;;; The arguments of the function are:
;;;; pred - name of the predicate to select tuples from
;;;; constraint - name of predicate which will constrain the pred
;;;; posns - the order in which the elements of each tuple in 'reln'
;;;;		has to exist in order to qualify in the 'constraint'
;;;; This function inputs a n-ary predicate 'pred' and a k-ary
;;;; predicate 'constraint' (k<=n). We have to find the list of
;;;; tuples in the relation from 'pred' such that it is constrained
;;;; by the tuples in the relation from 'constraint' at the positions
;;;; given by posns.
;;;; For example: reln (from pred) - ((a b c) (c b e) (b a e)),
;;;; constraint-reln (from constrain) - ((b a) (c a) (a e) (e b)),
;;;; posns - (3 1); then the output will be ((a b c)(b a e))		
(defun constrain-predicate (pred constraint posns)
(cond ((and (listp posns)(every #'numberp posns)(gethash pred *kb-ht*) 
		(or (gethash constraint *kb-ht*) (and (listp constraint)
		(= (length constraint) 2) (equal (car constraint) 'not)
		(gethash (cadr constraint) *kb-ht*)))) 
			(let ((lst) (reln) (constraint-reln) (cons-name)
					(check) (temp) (pred-name) (consLen)
			(ht (make-hash-table :test #'equal)))
				(setf reln (gethash pred *kb-ht*))
			(if (and (listp constraint) (equal (car constraint) 'not))
				;; Checking if it is +ve or -ve (NOT)
					(setf constraint-reln  
						(gethash (cadr constraint) *kb-ht*))
					(setf constraint-reln  
						(gethash constraint *kb-ht*)))
				(setf consLen (length (car constraint-reln)))
				(setf cons-name (caar constraint-reln))
				(setf pred-name (caar reln))
				(cond ((not (> (length (car constraint-reln))
										(length (car reln))))
					;; Checking if constraint is a k-ary predicate and
					;; pred is a n-ary predicate where k<=n
						(store-facts constraint-reln ht)
						(loop for x in reln do
							(setf temp (reorder x posns))
							(setf check (cons cons-name temp))
							(if (< (length check) consLen)
								(progn
									(setf temp (copy-list x))
									(setf check (cons cons-name 
									(cdr (getFact temp posns))))))
							(if (or	(and
										(listp constraint)
										(equal (car constraint) 'not)
										(not (gethash check ht)))
									(and (or
											(and
												(listp constraint)
									(not (equal (car constraint) 'not)))
											(symbolp constraint))		
										(gethash check ht)))
								(setf lst (append lst (list x)))))
						(setf lst (mapcar #'cdr lst))
					;; Updating list with the qualifying tuples of 'reln'
						lst) ;; Returning the list
(t 
(format t "No. of Constraint Arguments > No. of Predicate Arguments")))))
(t nil)))

;;;; NAME-AND-STORE-RELATION
;;;; Function that creates new entries for the global hash-table
;;;; *kb-ht* by adding new entries with the tuples generated from
;;;; 'constrain-predicate' and a newly generated predicate-name.
;;;; The arguments of the function are:
;;;; pred - name of the predicate to select tuples from
;;;; constraint - name of predicate which will constrain the pred
;;;; posns - the order in which the elements of each tuple in 'reln'
;;;;		has to exist in order to qualify in the 'constraint'
;;;; reln - the output of the 'constrain-predicate'
(defun name-and-store-relation (pred constraint posns reln)
	(cond	((and (listp posns)(every #'numberp posns)(listp reln)
			(listp (car reln)))
			;; Checking for validity in the inputs
	(let ((newpred) (constr) (predicate))
	;; Taking two local variables newpred and constr; newpred: stores
	;; the newly generated name of the new predicate, constr: stores
	;; the name of the constraint-predicate in string (since it might
	;; be either pred-name or (not pred-name)
		(if (listp pred)
			(setf predicate (format nil "~{~a~}" pred))
		;; If it is (not pred-name) it generates the string considering
			;; it to be a list, otherwise just converts the symbol-name
			(setf predicate (symbol-name pred)))		
		(if (listp constraint)
			(setf constr (format nil "~{~a~}" constraint))
		;; If it is (not pred-name) it generates the string considering
			;; it to be a list, otherwise just converts the symbol-name
			(setf constr (symbol-name constraint)))
		(setf newpred (read-from-string (concatenate 'string
		predicate "_" constr "_" (format nil "~{~a~}" posns))))
	;; Generating the new pred-name by concatenating the names of the two
		;; input predicates with the positions list
		(store-facts (mapcar (lambda (x) (cons newpred x)) reln) *kb-ht*)
;; Updating the reln tuples by adding the name of the predicate to the
;; beginning of each tuple and calling 'store-facts' from lisp1 to store
;; the facts in the global hash table *kb-ht*
	newpred))(t (format t "Invalid Input Entered!"))))
	
;;; Helper Function: This getVarPos function returns a list of
;;; positions where there are variables in the relation list 'reln'.
;;; The variables are prefixed by '?' by which it can be identified.
;;; For example, if the relation is (on b1 ?x) the returned positions
;;; list is '(2) since ?x is at the second position of the arguments
(defun getVarPos(reln)
	(if (equal (car reln) 'not) ;; Checking if negated fact or not
		(setf reln (cdr reln)))
		;; If yes, dropping the negation and getting the fact
	(let ((posns) (i 0))
	;; Initialising a variable i to 0 to store the positions
		(loop for x in (cdr reln) do
			(setf i (+ i 1))
			;; Increasing i on every iteration of loop by 1
			(if (equal (char (string x) 0) '#\?)
			;; Checking for variable
				(setf posns (append posns (list i)))))
				;; If variable, appending i
			posns))

;;; Helper Function: This changeReln function changes the fact in 
;;; 'reln' into one that can be referenced with the hash-table by
;;; taking care of the names of the predicate along with replacing
;;; variables with '-' to be able to check in the hash-table
(defun changeReln(reln)
	(if (symbolp reln)
		(return-from changeReln reln))
	;; If relation is just a symbol like 'ON' then returns itself
	(let ((newReln) (count 0) (refReln))
	;; Taking local variables to store the new relation and keep a count
	;; of the variables
		(if (equal (car reln) 'not)
			(setf refReln (cadr reln))
			(setf refReln reln))
		;; Checking for negation and storing the appropriate reln in 
		;; refReln (relation to be worked on)
		(loop for x in (cdr refReln) do
		;; Looping through the elements of refReln (ignoring name)
			(cond ((equal (char (string x) 0) '#\?)
				(setf newReln (append newReln (list '-)))
				(setf count (+ count 1)))
				;; If it is a variable, it sets the element at that
				;; position to - and increases count by 1
				(t (setf newReln (append newReln (list x))))))
				;; Otherwise just adds the normal element to newReln
		(if (= (length (cdr refReln)) count)
			;; Checks if all elements in the list were variables or not
			(setf newReln (car refReln))
			(setf newReln (cons (car refReln) newReln)))
			;; If yes, drops all elements and just sets newReln to name
			;; of predicate, otherwise adds the name of predicate to the
			;; created newReln
		(if (equal (car reln) 'not) (cons (car reln) (list newReln))
			newReln)))
			;; Checking if it was a negated fact, if yes, returns 
			;; (NOT (RELN)) otherwise just returns the (RELN)

;;; Helper Function: This getMainLiteral function finds the positive
;;; fact in the description which contains the max variables, this is
;;; the literal which all the other literal will be constrained against
(defun getMainLiteral (descr)
	(cond ((and (equal (car descr) 'and) (listp (cadr descr)))
	;; Checks if first element is 'and' and second is a list or not
		(let ((mainLiteral) (varPos))
		(loop for x in (cdr descr) do
		;; Looping through the description list (dropping the AND)
		(cond ((not (equal (car x) 'not))
		;; Not allowing negated facts to be the main literal
			(setf varPos (getVarPos x))
			;; Getting positions of variables in each relation
				(if (or
				(> (length varPos) (length (getVarPos mainLiteral)))
				(and
				(= (length varPos) (length (getVarPos mainLiteral)))
				(> (length x) (length mainLiteral))
				(not (= (length varPos) 0))))
				;; Setting mainliteral for fact containing maximum
				;; variables
				(setf mainLiteral x)))))
			mainLiteral))
		((> (length (getVarPos descr)) 0) 
			descr)
			;; If there are no variables, then return the descr itself
		(t	nil	)))

;;; Helper Function: This indexOf function returns the index of an
;;; element 'ele' in a list 'lst'
(defun indexOf (lst ele)
	(loop for x from 0 to (- (length lst) 1) do
	;; Looping through the list
		(if (equal ele (nth x lst))
		;; If found, returns the value of x
			(return-from indexOf x)
		))	nil)
		;; If not found, then returns nil

;;; Helper Function: This getPosns function finds the posns list posns
;;; list for using the function 'constrain-predicate'. It takes the
;;; main literal and an argument and finds where the varible in the
;;; argument lies in the main literal
(defun getPosns(main arg)	
	(let ((posns))
		(loop for x in (cdr arg) do
			(if (equal (char (string x) 0) '#\?)
			;; Checks if the element is a variable or not
				(setf posns (append posns (list (indexOf main x))))))
		posns))

;;; Helper Function: This checkValidity function checks if the input
;;; entered for the find-all-instances function is a valid input or not
(defun checkValidity (descr)
(if (and (listp descr) (= (length descr) 3))
	;; The input has to be a list of 3 elements (:L (?var) (descr))
	(progn	(if (and (equal (car descr) ':L)
	(every #'listp (cdr descr)))
	;; Checks if the first element is ':l' and the subsequent elements
	;; are all lists
	(progn	(let ((variables (cadr descr)) (lst (caddr descr)))
	;; Taking local variables storing the variables (the 2nd element of
	;; descr (hence the cadr)) and the lst of descr (the caddr)
		(loop for x in variables do
		(if (not (equal (char (string x) 0) '#\?))
			(return-from checkValidity nil)))
		;; Checking if all elements of variables is a variable or not
		;; Denoted by the prefix '?'
		(if (and (equal (cdddr descr) nil)
		(and (> (length variables) 0) (< (length variables) 3))
		(listp lst) (or (and (equal (car lst) 'not)
		(every #'symbolp (cadr lst)) (= (length (cdr lst)) 1))
		(and (equal (car lst) 'and)
		(every #'listp (cdr lst))(> (length (cdr lst)) 1))
		(every #'symbolp lst))(< (length (getMainLiteral lst)) 4))
		;; Doing checks as per the specifications to check for validity
		(return-from checkValidity t))))
		nil))nil))

;;;; FIND-ALL-INSTANCES
;;;; Function that retrives all the relations which holds according to 
;;;; the knowledge base or not.
;;;; The arguments of the function are:
;;;; descr - a positive predication or a list of and'ed predications
(defun find-all-instances (descr)
	(if (checkValidity descr)
	;; Checks for validity of descr by calling the checkValidity function
	(progn
		(let ((main (getMainLiteral (caddr descr))) (posns)
		(reln) (final) (finalList) (flag 0)
		(variables (cadr descr))(lst (caddr descr)))
		;; Storing values in local variables to be used in the function
		(if (equal main nil)
		;; Has to contain a predication with a lambda variable
		(progn
		(format t "None of the predications contain any lamba variable!")
		(return-from find-all-instances nil))
		(progn
		(if (not (equal (car lst) 'and))
		;; Single fact encountered
		(if (gethash (changeReln lst) *kb-ht*)
		;; Checks if it is present in the knowledge-base or not
		(setf finalList (mapcar #'cdr
		(gethash (changeReln lst) *kb-ht*)))
		;; Drops the name of the predicates and stores in final list
		(return-from find-all-instances nil))
		(progn
			(setf final main)
			(loop for x in (cdr lst) do
			;; Loops through the relations in lst
			(cond
				((not (equal x main))
				(if (equal (car x) 'not)
				(setf posns (getPosns main (cadr x)))
				(setf posns (getPosns main x)))
			;; Sets posns according to +ve or -ve fact and using getPosns
			(cond
				((equal posns nil)
				;; If posns is nil, it is a fact without a variable
				(if (equal (car x) 'not)
				(if (gethash (cadr x) *kb-ht*)
				(return-from find-all-instances nil))
				(if (not (gethash x *kb-ht*))
				(return-from find-all-instances nil))))
			(t
			;; Sets flag to 1 which shows that there exists a literal
			;; other than the main literal which contains a variable
			(setf flag 1)
			(setf reln (constrain-predicate (changeReln final)
			(changeReln x) posns))
			(setf final (name-and-store-relation (changeReln final)
			(changeReln x) posns reln))
			(setf finalList reln))))))
			;; Uses constrain-predicate and name-and-store-relation to
			;; create new predicate relations and store in hash-table
			(if (= flag 0)
			;; If flag is 0, there was no other literal with a variable
			;; other than the main literal
			(setf finalList (mapcar #'cdr (gethash
			(changeReln final) *kb-ht*))))))
			;; In that case, we get the relations of the main literal
			;; from the hash-table
			(setf posns (mapcar '1- (getPosns main
			(cons 'var variables))))
			(remove-duplicates (mapcar (lambda (x)
			(reorder x posns)) finalList) :test #'equal)))))
			;; Finding the lambda variables from the qualifying relations
		(format t "Invalid Input(s) Entered!")))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FUNCTIONS DEFINED FOR LISP ASSIGNMENT 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helper Function: This 'isTable' function takes an argument from
;;; the calling function namely:
;;; table - the argument to check if table or not
;;; The function returns if the argument passed is a table or not
(defun isTable(table)
	(if (and (> (length (symbol-name table)) 4)
		(equal (subseq (symbol-name table) 0 5) "TABLE"))
		t	nil))

;;;; 1. MARK-DEPTH
;;;; Function that recursively marks the block 'b' and its supporting
;;;; (i.e. the ones underneath) blocks with the corresponding depth
;;;; numbers.
;;;; Arguments of the Function:
;;;; a) b: The name of the block to be marked
;;;; b) d: The depth number for the corresponding block 'b'
(defun mark-depth(b d)
	(let ((reln (list 'on b '-)))
	;; Creates list (eg. on block -) which can be searched in *kb-ht*
		(if (or	(isTable b)			(equal b nil)
				(not (numberp d))	(not (gethash reln *kb-ht*)))
			;; Cases which return nil (aka: ends the function)
			;; a. The block is a 'table' (table does not have depth)
			;; b. The block is nil (invalid input case)
			;; c. 'd' is not a number (invalid input case)
			;; d. The block does not have an on relation in *kb-ht*
			nil
			(progn
				(setf (gethash b depth) d)
				;; Sets the depth-number of the block in 'depth' ht
				(setf reln (car (gethash reln *kb-ht*)))
				;; Retrieves the on's for the block from *kb-ht*
				(mark-depth (car (last reln)) (+ d 1))
				;; Calls the function recursively with the relation's
				;; last element (ie the block ON which 'b' lies)
				t))))

;;;; 2. SAME-STACK
;;;; Function that recursively checks if two blocks are part of the
;;;; same stack or not by comparing the depth numbers of both blocks
;;;; and checking if they eventually turn out to be the same block.
;;;; Arguments of the Function:
;;;; a) b1: The name of the first block
;;;; b) b2: The name of the second block	
(defun same-stack(b1 b2)
	(if (and (gethash b1 depth) (gethash b2 depth))	(progn
	(cond	((= (gethash b1 depth) (gethash b2 depth))
		;; Checks if the depths of b1 and b2 are equal or not
		(if (equal b1 b2) t nil))
		;; If they are the same block, it meant in same stack,
		;; otherwise it means they are of different stacks
	(t	(let ((newB1) (newB2) (reln))
		;; If depths are unequal, assign local variables
		(if (< (gethash b1 depth) (gethash b2 depth))
		;; Checks for block with smaller depth
		;; The block with the smaller depth is checked with the
		;; hash-table to find out which block it lies on, and that
		;; becomes the newB1, while the other is the newB2
		(progn
			(setf reln (car (gethash (list 'on b1 '-) *kb-ht*)))
			(setf newB1 (car (last reln)))
			(setf newB2 b2))
		(progn
			(setf reln (car (gethash (list 'on b2 '-) *kb-ht*)))
			(setf newB1 (car (last reln)))
			(setf newB2 b1)))
		(if (or (isTable newB1) (isTable newB2))
			(return-from same-stack nil))
		;; Returns nil if either becomes the table. Using
		;; 'return-from' to cover for bad-input cases only,
		;; otherwise the above check would not be required
		;; as anyway whenever 'table' is checked in 'depth'
		;; hash-table, it would return a nil
		(same-stack newB1 newB2)))))
		;; Calls the function recursively with newB1 and newB2
	(format t "Input(s) do not have depth numbers. Invalid.")))

;;;; 3. COST
;;;; Function that computes the cost of stacking up two blocks where
;;;; the first argument is to be stacked on the second argument.
;;;; Arguments of the Function:
;;;; a) b: The name of the block to be stacked
;;;; b) x: The name of the second block/table to be stacked on	
(defun cost(b x)
(if (and (gethash b depth) (or (gethash x depth) (isTable x))) (progn
;; Checks if the inputs are blocks with depths or 'x' is a table or not
	(let ((cost) (reln (car (gethash (list 'on b '-) *kb-ht*))))
	;; Storing the relation which can be checked with the hash-table
	(cond ((equal b x)
			nil)	;; Not possible to stack a block on itself	
		((isTable x) ;; If 'x' is a table, checks if 'b' is
			(if (isTable (car (last reln))) ;; already on a table
				(setf cost 0)
				(setf cost (+ (gethash b depth) 1))))
		((equal (car (last reln)) x) ;; Checks if 'b' is already on 'x'
			(setf cost 0))
		((and (= (gethash b depth) 0) (= (gethash x depth) 0))
			(setf cost 1)) ;; Checks if 'b' and 'x' are both clear
		((same-stack b x) ;; Checks if 'b' and 'x' are of the same stack
			(if (> (gethash b depth) (gethash x depth))
				(setf cost (+ (gethash b depth) 1))
				(setf cost (+ (gethash x depth) 1))))
		(t	(setf cost (+ (gethash b depth) (gethash x depth) 1))))
	cost)) ;; Returns the cost
	(format t "Invalid Input. Has to be block-block / block-table.")))

;;; Helper Function: This 'getBlocks' function takes an argument from
;;; the calling function namely:
;;; descr - a description of a list of blocks or a list of block(s)
;;; The function returns a list of blocks that match the knowledge base
;;; with the description of the list, or returns the list or block
;;; itself if needed.	
(defun getBlocks(descr)
(if (listp descr) (if (equal (car descr) ':L)
;; Checks if a list, if yes, checks if its a description, if yes, calls
;; find-all-instances on it, if not, returns the list. If not a list,
;; returns the atom, but in the format of a list
	(mapcar #'car (find-all-instances descr)) descr)
	(list descr)))

;;;; 4. STACKING-CANDIDATES
;;;; Function that returns a list of stacking candidates in an
;;;; ascending order of their costs.
;;;; Arguments of the Function:
;;;; a) descr1: Description of first list of blocks
;;;; b) descr2: Description of second list of blocks	
(defun stacking-candidates(descr1 descr2)
(let ((final) (lst1 (getBlocks descr1)) (lst2 (getBlocks descr2)))
;; Stores the lists by calling getBlocks on both descr1 and descr2
(if (and lst1 lst2 (every #'symbolp lst1) (every #'symbolp lst2))
(progn (loop for x in lst1 do	(loop for y in lst2 do (if (cost x y)
	;; Loops through the two lists, and if cost exists between them
	(setf final (append final (list (list x y (cost x y))))))))
	;; Appends the list of x, y and cost to the final list
	(sort final #'< :key #'caddr)) ;; Sorts according to cost & returns
	(format t "Description(s) did not match any blocks.~%")))) 