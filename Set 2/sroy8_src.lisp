;;;; Lisp Problem Set 1
;;;; Sayudh Roy

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
	(setf (gethash pred ht) (append (gethash pred ht) val))
	(setf (gethash pred ht) val))
)
	
;;; Helper Function: This 'indexOf' function takes two arguments
;;; from the calling function namely:
;;; lst - a list of elements
;;; ele - an element which is to be searched for in the list
;;; pos - recursively increases pos to return index of ele in lst
;;; The function recursively checks through the list to search
;;; for 'ele' in the list and if present returns the position of
;;; the element, and if not, it returns nil
(defun indexOf (lst ele pos)
	;; Checking if the element is equal to the first element 
	(if (equal ele (car lst))(return-from indexOf pos))
	;; Checking if lst is a list or not, if it isn't it means
	;; that element is not present in the lst, it has recursively
	;; checked the list and did not find a match, hence returns
	;; nil, else it calls 'indexOf' again with pos increased by 1
	(if lst (indexOf (cdr lst) ele (+ pos 1))))

;;; Helper Function: This 'order' function takes two arguments 
;;; from the calling function, namely:
;;; all-vars - a list with 'n' elements
;;; some-vars - a definite subset of all-vars in any order
;;; The function calls the indexOf function and finds the index
;;; of each of the elements in 'some-vars' in 'all-vars' and
;;; creates a list of the indices.
;;; For example, if 'all-vars' is (x y z) and 'some-vars' is 
;;; (z x), the function	returns (2 0)
(defun order(all-vars some-vars)
	(let ((lst))
		;; We are taking a local variable 'lst' which will store
		;; the indices of the elements  of some-vars in the order
		;; in which they appear in all-vars
		(loop for x from 0 to (- (length some-vars) 1) do
		;; we call the indexOf function to find the index of the
		;; element and append it to 'lst'
			(setf lst (append lst (list (indexOf all-vars 
								(nth x some-vars) 0)))))
		(return-from order lst)))

;;;;1. Function that extracts a non-redundant list of constants
;;;; from a list of atomic facts.
;;;; lst - used to store the list of facts entered by the user
;;;; This function uses the mapcar function to create a list of
;;;; constants extracted from the facts entered by the user and
;;;; then uses the remove-duplicates function to return a modified
;;;; copy of the list from which any element that matches another
;;;; element occurring in list has been removed
(defun extract-constants (lst)
	(remove-duplicates(apply #'append
		(mapcar (lambda (x) (cdr x)) lst))))



;;;;2. Function to store a fact entered by the user in multiple
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
					(update-table (cons (car fact) c) fact ht)))))))
					;; Adds each of these updated arguments' lists
					;; appended to the predicate as an entry for the
					;; hash table
		(loop for key being the hash-keys of ht collect key))
		;; Returns the keys that were added to the hash-table

;;;; 3. Functions that store facts in a hash-table and extracts
;;;; the constants from those facts making a list of these
;;;; constants the value of a global parameter "domain"
;;;; ht - the hash table to store the facts with their keys
;;;; facts - used in the 'store-facts' function to take all the
;;;; facts and pass it one by one to the 'store-fact' function to
;;;; add records to the hash table
;;;; NOTE: For this function to run a hash-table has to be created
;;;; which can be created using the following input:
;;;; (defparameter ht (make-hash-table :test #'equal))
;;;; For my test cases the hash table is created before the function is
;;;; executed
(defparameter *domain* nil)
(defun store-facts(facts ht)
	;; This function takes as input a series of facts and a
	;; hash-table ht and using mapcar, maps them to the function
	;; above to store each of them in the hash table 'ht'
	(mapcar (lambda (x) (store-fact x ht)) facts)
	;; Declares a global parameter domain in which it stores just
	;; the constants from all the facts entered in a irredundant
	;; way by calling the extract-constants functions from part 1
	(setf *domain* (extract-constants facts))
	;; Returns the domain from the function (an irredundant list
	;; of constants) 
	(return-from store-facts *domain*)
)

;;;;4. A function that projects an n-dimensional relation 'reln',
;;;; producing a k-dimensional relation with 0<k<n.
;;;; reln - list of sub-lists each of n elements entered by user
;;;; all-vars - list of variable names corresponding to the
;;;; 			elements of the n-tuples
;;;; some-vars - subset of all-vars in some order
;;;; This function iterates over the 'reln' list and reorders each
;;;; of the sub-lists into the order specified by some-vars. The 
;;;; order is fetched by calling the 'order' helper function and
;;;; the list is created and returned accordingly.
(defun project-relation (reln all-vars some-vars)
	;; Taking local variables lst, lst1 and lst2 for:
	;; lst: calls the 'order' function to get a list of the order
	;;		in which the some-vars are arranged from all-vars
	;; lst1: for each of the sublist in reln, lst1 stores the
	;;		elements in the order in which some-vars are
	;; lst2: stores the final irredundant list of reln with
	;;		elements in the order of some-vars
	(let ((lst (order all-vars some-vars)) (lst1) (lst2))
		(loop for x in reln do	;; Taking each sublist at a time
			(loop for y in lst do
				(setf lst1 (append lst1 (list (nth y x)))))
			(if (not (indexOf lst2 lst1 0))
			;; This check will check if lst1 is already an element
			;; in lst2 or not. If it is, it will return a valid
			;; index from lst2. If it returns nil, it means that it
			;; is not present in the list, and thus is not a
			;; redundant entry to the list.
				(setf lst2 (append lst2 (list lst1))))
			;; Resetting lst1 to nil to store the values of the
			;; next sublist of reln in it
			(setf lst1 ()))(return-from project-relation lst2)))

;;;;5. A function which takes the following arguments from the user
;;;; reln - A list of sub-lists of elements entered by the user
;;;; constr - A list of elements to be found at position 'posn' 
;;;; in each of the sub-lists  of the 'reln'
;;;; posn - Position of where each of the 'constr' have to exist
;;;; in 'reln'
;;;; This function checks for each of the elements in constraints
;;;; if the same element exists at position 'posn' in 'reln'. If
;;;; it does, it creates a list of all those sub-lists in 'reln'
;;;; and returns it
(defun constrain-relation (reln constr posn)
	(let ((lst))
	;; We are using a local list variable 'lst' to store the
	;; matches from 'reln'
		(loop for x in constr do
			(loop for y in reln do
				(let ((i (indexOf y (car x) 0)))
				;; In a local variable 'i' we store the value of
				;; the index of the element of constr in the
				;; current sublist (y) of reln by calling the
				;; helper function indexOf
					(if (and i (= i (- posn 1)))
					;; If it is present in reln and at the position
					;; specified by posn, it adds to the 'lst'
					;; variable
						(setf lst (append lst (list y)))))))
	(return-from constrain-relation lst)))