; Recursive Descent Parser for the Grammar:
; I  -> iES'
; S' -> S | SeS
; E  -> GE'
; E' -> oGE' | ε
; G  -> x | y | z | w
; S  -> s | dLb
; L  -> s | sL

(defun Gfn (str)
  (print '( in Gfn)) (print str)
  (cond ; Check if the first character is a valid terminal symbol (x, y, z, w)
    ((or (eq (car str) 'x) (eq (car str) 'y) (eq (car str) 'z) (eq (car str) 'w))
     (cdr str)) ; Consume the terminal and return the rest of the string
    (t (append str (list 'err)))))  ; Else, return an error

(defun Lfn (str)
  (print '( in Lfn)) (print str)
  (cond 
    ((eq (car str) 's) ; Check if the first character is 's'
     (let ((x (Lfn (cdr str)))) ; Recursively parse more 's' if they exist
       (if (null x) 
           (append (cdr str) (list 'err)) ; Return error if parsing fails
           x))) ; Else, return the rest of the string
    (t str))) ; Else, return as is (empty(ε)-case)

(defun Sfn (str)
  (print '( in Sfn)) (print str)
  (cond 
    ((eq (car str) 's) (cdr str)) ; If 's', consume and return rest
    ((eq (car str) 'd)
     (let ((x (Lfn (cdr str)))) ; Parse L after 'd'
       (if (null x) 
           (append (cdr str) (list 'err)) ; Error if parsing fails
           (if (eq (car x) 'b) (cdr x)  ; If 'b' follows, consume it
               (append x (list 'err)))))) ; Otherwise, return error
    (t (append str (list 'err)))))  ; If no valid match, return error

(defun Sfn-prime (str) ; Handles the optional eS case
  (print '( in Sfn-prime)) (print str)
  (cond 
    ((null str) str)  ; If empty, return as is (ε-case)
    ((eq (car str) 'e)  ; If 'e', consume and continue parsing S
     (let ((x (Sfn (cdr str))))
       (if (null x) 
           (append (cdr str) (list 'err)) ; Return error if parsing fails
           x))) ; Else, return rest
    (t (Sfn str)))) ; Else, just parse S

(defun Efn-prime (str)  ; Handles the optional oG E' case
  (print '( in Efn-prime)) (print str)
  (cond 
    ((null str) str)  ; If empty, return (ε-case)
    ((eq (car str) 'o)  ; If 'o', consume and parse G
     (let ((x (Gfn (cdr str))))
       (if (null x) 
           (append (cdr str) (list 'err)) ; Return error if parsing fails
           (Efn-prime x)))) ; Else, continue parsing E'
    (t str))) ; Else, return as is (empty(ε)-case)

(defun Efn (str)
  (print '( in Efn)) (print str)
  (let ((x (Gfn str)))  ; Parse G and then handle E'
    (if (null x) 
        (append str (list 'err))  ; If G fails, return error
        (Efn-prime x))))  ; Else, continue parsing E'

(defun Ifn (str)
  (print '( in Ifn)) (print str) 
  (cond 
    ((eq (car str) 'i)  ; Check if input starts with 'i'
     (let ((x (Efn (cdr str)))) ; Parse E
       (if (null x) 
           (append (cdr str) (list 'err)) ; Error if E fails
           (Sfn-prime x)))) ; Else, continue parsing S'
    (t (append str (list 'err)))))  ; Else, 'i' is missing, throw error

(defun parse (input)  ; Function caller
  (Ifn input))
