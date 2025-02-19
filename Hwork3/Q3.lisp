; Question 3 - "Bottom-up" Mergesort
(defun my-merge (left right) ;; my-merge function from question 2
  (cond
    ((null left) right)  ;; If left is empty, return right
    ((null right) left)  ;; If right is empty, return left
    ((<= (car left) (car right))  ;; If left's first element is smaller, take left's first element and make recursive call
     (cons (car left) (my-merge (cdr left) right))) 
    (t  ;; If right's first element is smaller, take right's first element and make recursive call
     (cons (car right) (my-merge left (cdr right))))))

(defun make-pairs (lst)
  (cond
    ((null lst) '()) ;; If the list is empty, return an empty list
    ((null (cdr lst)) (list (list (car lst)))) ;; If one element is left, put it into a sublist
    (t (cons (list (min (car lst) (cadr lst)) (max (car lst) (cadr lst))) ;; Create a sorted pair
             (make-pairs (cddr lst)))))) ;; Make recursive call for the rest of the list

(defun merge-pairs-helper (lst) ;; A helper is the only way I could figure out how to work this without built-in loop function
  (cond
    ((null lst) '())  ;; If empty, return empty list
    ((null (cdr lst)) (list (car lst)))  ;; If an odd element remains, keep it as is
    (t (cons (my-merge (car lst) (cadr lst))  ;; Merge first two lists using my-merge function I defined in question 2
             (merge-pairs-helper (cddr lst))))))  ;; Make recursive calls for remaining lists

(defun merge-pairs (lst)
  (cond
    ((null lst) '())  ;; If the list is empty, return an empty list
    ((null (cdr lst)) (car lst))  ;; If only one sublist remains, return it as the final sorted list
    (t (merge-pairs (merge-pairs-helper lst)))))  ;; Make recursive calls to merge pairs

(defun bottom-up-mergesort (lst)
  (merge-pairs (make-pairs lst))) ;; Start with creating the sorted pairs and then merge them after