

(use-modules (ice-9 textual-ports)
	     ; (ice-9 pretty-print)
	     (srfi srfi-1)
	     (srfi srfi-43)
	     (syntax threading))

(define ++
  (lambda (x)
    (+ x 1)))

(define tokenize
  (lambda (file)
    "given string FILE, return the text contents as a vector of words
separated by whitespace"
    (with-input-from-file file
      (lambda ()
	(~> (get-string-all (current-input-port))
	    (string-split <> char-set:whitespace)
	    (list->vector <>))))))

(define remove-non-alphabet
  (lambda (str)
    "remove all non-alphabetic characters from a string"
    (let loop ((res '())
	       (ls (string->list str)))
      (cond ((null? ls) (list->string (reverse res)))
	    ((char-alphabetic? (car ls))
	     (loop (cons (car ls) res)
		   (cdr ls)))
	    (else (loop res (cdr ls)))))))

(define sanitize-input
  (lambda (ls)
    (~> (map remove-non-alphabet ls)
	(remove (lambda (x)
		  (zero? (string-length x)))
		<>))))

(define unique
  (lambda (vec)
    "Return a vector containing all the unique words from input VEC"
    (let loop ((memo '())
	       (index 0))
      (cond ((= index (vector-length vec))
	     (list->vector (reverse memo)))
	    ((not (member (vector-ref vec index) memo))
	     (loop (cons (vector-ref vec index) memo)
		   (++ index)))
	    (else (loop memo (++ index)))))))

(define vector-member
  (lambda* (vec obj #:optional (pred equal?))
    (let loop ((index -1)
	       (found? #f))
      (cond ((= (vector-length vec) (++ index)) #f)
	    ((pred (vector-ref vec (++ index)) obj)
	     (++ index))
	    (else (loop (++ index) found?))))))

(define word-occurences
  (lambda (str vec vec-unique)
    "return a vector containing the occurences of STR in VEC beside words
in a given vector VEC-UNIQUE"
    (let loop ((result (make-vector (vector-length vec-unique) 0))
	       (index 0))
      (cond ((= index (- (vector-length vec) 1)) result)
	    ((string-ci=? str (vector-ref vec (++ index)))
	     (let* ((word (vector-ref vec index))
		    (word-index (vector-member vec-unique word string-ci=?))
		    (inc-val (++ (vector-ref result word-index))))
	       (begin (vector-set! result word-index inc-val)
		      (loop result (++ index)))))
	    (else (loop result (++ index)))))))

(define occurences-matrix
  (lambda (vec vec-unique)
    "return a two-dimensional array containing the occurences of a
word in VEC-UNIQUE beside words in VEC.

For example:

Say the word \"sauce\" shows up in index 19 in VEC-UNIQUE, we then
checking if it begins showing up beside (i + 1) words in VEC, and
increment it every time it does."
    (vector-map (lambda (i v)
		  (word-occurences v vec vec-unique))
		vec-unique)))

(define get-row-sum
  (lambda (vec)
    "given a vector VEC whose contents are all numbers, return the sum of
all numbers."
    (let loop ((result 0)
	       (index 0))
      (if (= index (- (vector-length vec) 1))
	  result
	  (loop (+ (vector-ref vec index) result)
		(++ index))))))

(define first-two-decimal-places
  (lambda (num)
    (let ((str (number->string num)))
      (if (> (string-length str) 3)
	  (string->number (string-take str 4))
	  (string->number (string-take str 3))))))

(define normalize-row
  (lambda (vec)
    "Sum all the numbers in VEC, and then return a new vector containing
VEC's contents divided by the sum."
    (let loop ((sum (get-row-sum vec))
	       (index 0)
	       (result '()))
      (if (= index (- (vector-length vec) 1))
	  (list->vector (reverse result))
	  (loop sum
		(++ index)
		(cons (~> (vector-ref vec index)
			  (/ <> sum)
			  (exact->inexact <>)
			  (first-two-decimal-places <>))
		      result))))))

(define normalize-matrix
  (lambda (matrix)
    "Apply `normalize-row` to all members of MATRIX and return a new
matrix."
    (let loop ((result '())
	       (index 0))
      (if (= index (- (vector-length matrix) 1))
	  (list->vector (reverse result))
	  (loop (cons (normalize-row (vector-ref matrix index)) result)
		(++ index))))))

(define file->raw-image
  (lambda (file)
    (let* ((tokens (~> (tokenize file)
		       (vector->list <>)
		       (sanitize-input <>)
		       (list->vector <>)))
	   (uniq-tokens (unique tokens)))
      `(,uniq-tokens
	. ,(~> (occurences-matrix tokens uniq-tokens)
	       (normalize-matrix <>))))))

(define scan-for-non-zero
  (lambda (vec)
    "Given VEC, return a list where each item is in the form
`(CONTENT . INDEX)` where INDEX is the index in the vector where
CONTENT is found."
    (let loop ((result '())
	       (index 0))
      (cond ((= index (- (vector-length vec) 1))
	     (reverse result))
	    ((not (zero? (vector-ref vec index)))
	     (loop (cons `(,(vector-ref vec index) . ,index)
			 result)
		   (++ index)))
	    (else (loop result (++ index)))))))

(define compress-matrix-image
  (lambda (matrix-image)
    "given a raw MATRIX-IMAGE, such as the ones output by
`file->raw-image`, compress the weights in such a way that the large,
mostly-zero-value matrix is replaced by a vector of lists containing
weights and "
    (let loop ((result '())
	       (index 0))
      (if (= index (- (vector-length (car matrix-image)) 1))
	  `(,(car matrix-image) . ,(reverse result))
	  (loop (cons (scan-for-non-zero
		       (vector-ref (cdr matrix-image) index))
		      result)
		(++ index))))))

(define matrix-image-ref
  (lambda* (matrix obj #:optional (pred equal?))
    (let ((index (vector-member (car matrix) obj pred)))
      (if index
	  `(,(vector-ref (car matrix) index) .
	    ,(vector-ref (cdr matrix) index))))))

