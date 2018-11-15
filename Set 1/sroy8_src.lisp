;;;;Lisp Problem Set 0
;;;;Sayudh Roy

;;;;1. Function to return t if item is in list, else returns nil
;;; x - element being looked for
;;; y - list in which it is being looked in
;;; Recursively checks if the element is present or not
(defun occur-in (x y)
	(if (equal x y)
			;;Found the element
			(return-from occur-in t)
		(if (and (listp y) y)
			;;Checking to see if y is still a list and if y still has an element
			(or (occur-in x (car y)) (occur-in x (cdr y)))
			;;Recursively calling the function either using the first element of
			;;the list or the remaining portion of the list, since it maybe present
			;;in either of the cases
		)
	)
	;;If not found, then the function will return nil at the end
)

;;;; 2. Function to differentiate single-variable monomial
;;; mono - the monomial which is being differentiated
;;; c - local variable to store the value of the first item i.e. the constant
;;; i - local variable to store the value of the third item i.e. the power
;;; Function returns a list with the differentiated values
(defun deriv1 (mono)
	(cond ((and (listp mono) (= 3 (length mono)) (symbolp (second mono))
			(numberp (first mono)) (numberp (third mono)))
			;; This is to check if the user's inputs are valid or not
			(let ((c (first mono)) (i (third mono))) 
				;; Taking local variables c and i to store the values
				(cond ((or (= c 0) (= i 0)) 
						;; Checking for condition as to if either c or i is 0
						(setq c 0)
						(setq i 0))
					(t
						;; Else performing the operations for differentiation
						(setq c (* c i)) 
						(setq i (- i 1))))		
				(setf (first mono) c) 	
				(setf (third mono) i)
				;; Changing the values in the list to the calculated value
				(return-from deriv1 mono))) 
				;; Returning the calculated list
		(t
			;; Incase the user inputs a wrong type (in the monomial)
			"Error! Wrong Input Type!"
)))

;;;; 3. Function to find the list of atoms comprising the leaves of the tree
;; tree - the list to be returned with just the list of atoms
(defun tree-yield(tree)
	(if (and (listp tree) tree)
		;; To check if the tree is a list and if it isn't empty, if yes it
		;; goes to the end and returns the tree itself
		(if (atom (car tree))
			;; Checks if first element is atom or not, if so then that element
			;; cannot be broken down further and hence can be joined with the 
			;; function call of the rest of the tree.
			(cons (car tree) (tree-yield (cdr tree)))
			;; Else the first element has to be further broken down (since it was
			;; a list in this case) and then applied the recursive funtion on that
			;; and used the append function to join with the function call of the
			;; rest of the tree
			(append (tree-yield (car tree)) (tree-yield (cdr tree))))
	tree
	;; To be returned in case the first check does not qualify
))

;;;; 4. Function that finds the first match of a predicate to an element of the facts list
;;; pred - predicate which has to be searched on the facts list
;;; facts - the list of facts that is to be entered
;;; Finds the first occurence of the matched predicate and returns the corresponding values
;;; for the match, thus the variables of the pred matched with those of the facts
(defun find-match (pred facts)
	(loop for item in facts do
	;; Going through all the items in the facts list
		(if (and (= (length pred) (length item)) (equal (car pred) (car item)))
			;; Finding a match checking if the length of the predicate is equal to
			;; that of the item AND if the first element (the predicate) matches with
			;; first element of the fact
			(if (= (length pred) 1)
				;; This is to check for predicates with zero arguments, incase it exists
				;; it will return T to show that it does, else return NIL
				(return-from find-match T)
				(return-from find-match (mapcar #'list (cdr pred) (cdr item)))
				;; Uses mapcar to make a list of the values determined by their matches
))))

;;;;5. Functions that store facts in a hash-table.
;; ht - the hash table to store the facts with their keys
;; fact - used in the 'store-fact' function to take each fact and store it
;; in the hash table according to the requirement
;; facts - used in the 'store-facts' function to take all the facts and pass 
;; it one by one to the 'store-fact' function to add records to the hash table
(defparameter ht (make-hash-table :test #'equal))
(defun store-fact(fact ht)
	(if (gethash (car fact) ht)
		;; This will check if there already exists a fact in the hash table with the same
		;; key. If it does, then the fact will just get appended to the existing fact
		(progn	(setf (gethash (car fact) ht) (append (gethash (car fact) ht)(cdr fact)))
				(setf (gethash fact ht) t)) ;; This is for the other format of updating
											;; the hash table by storing the entire fact
											;; as a key and its value to be t
		(progn	(setf (gethash (car fact) ht) (cdr fact))
				;; This stores the key value (the first part of the fact) in the hash 
				;; table with the corresponding value which is found in the second part
				;; of the fact
				(setf (gethash fact ht) t))))
				;; Also it stores the fact as a whole as a key of the hash table with its
				;; corresponding value to be t
(defun store-facts(facts ht)
	;; This function takes as input a series of facts and a hash-table ht and using mapcar,
	;; maps them to the function above to store each of them in the hash table 'ht'
	(mapcar (lambda (x) (store-fact x ht)) facts))