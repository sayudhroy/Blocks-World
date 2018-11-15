;;;; Lisp Problem Set 3
;;;; Sayudh Roy

(defparameter *kb-ht* (make-hash-table :test #'equal))
;; Declaring the global hash-table 'knowledge base'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE FOLLOWING FUNCTIONS HAVE BEEN TAKEN FROM THE LISP 2 ASSIGNMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	(setf (gethash pred ht) (list val)))
)
	
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
;;;; For my test cases the hash table is created before the function is
;;;; executed
(defun store-fact(fact ht)
	(cond ((gethash fact ht)
		;; Checks if the fact is already present in the hash-table,
		;; if yes, it shows an error and exits
		nil)
		(t
		(update-table (car fact) fact ht)
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
;;;; NOTE: Although the 'store-facts' in Lisp1 used a domain to store
;;;; the constants of the kb, this function does not have that, since
;;;; for the purposes of Lisp2, I do not need to store the domain,
;;;; hence I did not want to use an extra global parameter (*domain*),
;;;; and an extra function (extract-constants) for the same.
;;;; ht - the hash table to store the facts with their keys
;;;; facts - used in the 'store-facts' function to take all the
;;;; facts and pass it one by one to the 'store-fact' function to
;;;; add records to the hash table
(defun store-facts(facts ht)
	;; This function takes as input a series of facts and a
	;; hash-table ht and using mapcar, maps them to the function
	;; above to store each of them in the hash table 'ht'
	(mapcar (lambda (x) (store-fact x ht)) facts)
)

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
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NEW FUNCTIONS FOR THE LISP 3 ASSIGNMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
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

;;;; QUESTION 1: FIND-WHETHER
;;;; Function that checks if the predications given in the 'descr' input
;;;; holds according to the knowledge base or not
;;;; The arguments of the function are:
;;;; descr - a positive predication or a list of and'ed predications
(defun find-whether (descr)
	(let ((main) (posns) (reln) (final))
	(cond ((and
		(listp descr)
		(or (and (equal (car descr) 'and)
			(every #'listp (cdr descr))
			(> (length (cdr descr)) 1))
			(every #'symbolp descr)
			(and (equal (car descr) 'not)
			(every #'symbolp (cadr descr))
			(= (length (cdr descr)) 1))
		) (< (length (getMainLiteral descr)) 4))
		;; Checking for the validity of the inputs, if yes goes on,
		;; otherwise returns invalid input
		(setf main (getMainLiteral descr))
		;; Gets the 'mainLiteral' by calling the function. A mainliteral
		;; is that literal which contains all the variables in question
		;; and is a positive literal. Every 'descr' has to have one if
		;; there are variables in them (as per RESTRICTIONS in preamble)
		(cond ((equal main nil)
		;; If nil, it means that all of the facts contain constants
			(if (or (not (equal (car descr) 'and))
			(not (listp (cadr descr))))
			;; Checks for descr to contain a single fact without vars
				(if (equal (car descr) 'not)
				;; If it is a negated fact, then we check if it does not
				;; exist in the hash table
				(if (gethash (cadr descr) *kb-ht*)
				(return-from find-whether nil)
				(return-from find-whether t))
				(if (gethash descr *kb-ht*)
				;; If it is a positive fact, we check if it exists in
				;; the hash table
					(return-from find-whether t)
					(return-from find-whether nil))))
				;; Otherwise proceed to checking each of the literals
				;; which don't have variables to check with the kb-ht
				(loop for x in (cdr descr) do
				(if (equal (car x) 'not)
					(if (gethash (cadr x) *kb-ht*)
						(return-from find-whether nil))
					(if (not (gethash x *kb-ht*))
						(return-from find-whether nil))))
						(return-from find-whether t))
				;; Checks if it is a negated fact or not, if negated, 
				;; returns nil if present otherwise
				;; returns nil if absent
				;; Below starts the section where there are variables
				;; present in the descr
					(t	(if (not (equal (car descr) 'and))
						(if (gethash (changeReln descr) *kb-ht*)
						(return-from find-whether t)
						(return-from find-whether nil)))
						;; If 'and' is not present, it means a single
						;; fact, so checks if present in knowledge base
						(setf final main)
						;; Final is set to the main literal
						(loop for x in (cdr descr) do
						;; Looping through the literals of descr
						(cond ((not (equal x main))
							(if (equal (car x) 'not)
							;; Checks if it is a negated literal
							(setf posns (getPosns main (cadr x)))
							(setf posns (getPosns main x)))
							;; If yes, the get positions with the Cadr
							(cond ((equal posns nil)
								(if (equal (car x) 'not)
									(if (gethash (cadr x) *kb-ht*)
										(return-from find-whether nil))
									(if (not (gethash x *kb-ht*))
										(return-from find-whether nil))))
							;; If posns is nil, there are no variables in
							;; x so it does a regular hash-lookup
			(t
			(setf reln (constrain-predicate (changeReln final)
			(changeReln x) posns))
			(setf final (name-and-store-relation (changeReln final)
			(changeReln x) posns reln)))))))
			;; Otherwise uses constrain-predicate and
			;; name-and-store-relation to store the new constrained
			;; relations
			(if (gethash (changeReln final) *kb-ht*)
				t
				nil))))
			(t (format t "Invalid Input(s) Entered!")))))

			
;;;; QUESTION 2: FIND-ALL-INSTANCES
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