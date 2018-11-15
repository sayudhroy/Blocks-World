;;;; Lisp Problem Set 2
;;;; Sayudh Roy

(defparameter *kb-ht* (make-hash-table :test #'equal))
;; Declaring the global hash-table 'knowledge base'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE FOLLOWING FUNCTIONS HAVE BEEN TAKEN FROM THE LISP 1 ASSIGNMENT
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
		(format t "THE FACT '~a' IS IN HT ALREADY!~%" fact))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NEW FUNCTIONS FOR LISP 2 ASSIGNMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;1. Function that selects a list of tuples from a relation
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
		(or (gethash constraint *kb-ht*)
		(and (listp constraint)(= (length constraint) 2) 
		(equal (car constraint) 'not)(gethash (cadr constraint) *kb-ht*))))
			;; Checks if 'pred' AND 'constraint' is stored in kb-ht
			;; The 'constraint' can be entered as a postive or a negative
			;; constraint. In case it is negative, necessary checks are in
			;; place to check its validity and existence in the kt-hb
			(let ((lst) (reln) (constraint-reln)
				(ht (make-hash-table :test #'equal)))
			;; Local variables to store final list (lst), tuples from the
			;; 'pred' predicate (reln), tuples from the 'constraint'
			;; predicate (constraint-reln) and a local hash table for the
			;; hash-lookup of matching tuples to get final list
				(setf posns (mapcar '1- posns))
				;; Adjusting the index of posns
				(setf reln (mapcar #'cdr (gethash pred *kb-ht*)))
				;; Storing the cdrs of the tuples from 'pred' predicate
				;; from the global hash-table kb-ht in 'reln'
				;; Cdrs are taken only to drop the predicate name
				(if (listp constraint) ;; Checking if it is +ve or -ve (NOT)
					(setf constraint-reln (mapcar #'cdr 
						(gethash (cadr constraint) *kb-ht*)))
					(setf constraint-reln (mapcar #'cdr 
						(gethash constraint *kb-ht*))))
				;; Storing the cdrs of the tuples from 'constraint' predicate
				;; from the global hash-table kb-ht in 'constraint-reln'
				;; Cdrs are taken only to drop the predicate name
				(cond ((not (> (length (car constraint-reln))
										(length (car reln))))
					;; Checking if constraint is a k-ary predicate and pred is a
					;; n-ary predicate where k<=n
						(mapcar (lambda (x)
							(setf (gethash x ht) t)) constraint-reln)
						;; Storing all the tuples with the predicates dropped in
						;; the local hash-table ht for hash-lookup
						(loop for x in reln do
							(if (or (and (listp constraint)
							(not (gethash (reorder x posns) ht)))
							(and (not (listp constraint))
							(gethash (reorder x posns) ht)))
								(setf lst (append lst (list x)))))
						;; Updating list with the qualifying tuples of 'reln'
						lst) ;; Returning the list
	(t (format t "No. of Constraint Arguments > No. of Predicate Arguments")))))
(t (format t "Invalid Input Entered!"))))

;;;;2. Function that creates new entries for the global hash-table
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
			(listp (car reln))(symbolp pred)(or (symbolp constraint)
			(and (listp constraint)(= (length constraint) 2)
			(equal (car constraint) 'not))))
			;; Checking for validity in the inputs
	(let ((newpred) (constr))
	;; Taking two local variables newpred and constr; newpred: stores
	;; the newly generated name of the new predicate, constr: stores
	;; the name of the constraint-predicate in string (since it might
	;; be either pred-name or (not pred-name)
		(if (listp constraint)
			(setf constr (format nil "~{~a~}" constraint))
			;; If it is (not pred-name) it generates the string considering
			;; it to be a list, otherwise just converts the symbol-name
			(setf constr (symbol-name constraint)))
		(setf newpred (read-from-string (concatenate 'string
		(symbol-name pred) "-" constr "-" (format nil "~{~a~}" posns))))
		;; Generating the new pred-name by concatenating the names of the two
		;; input predicates with the positions list
		(store-facts (mapcar (lambda (x) (cons newpred x)) reln) *kb-ht*)
		;; Updating the reln tuples by adding the name of the predicate to the
		;; beginning of each tuple and calling 'store-facts' from lisp1 to store
		;; the facts in the global hash table *kb-ht*
	newpred))(t (format t "Invalid Input Entered!"))))