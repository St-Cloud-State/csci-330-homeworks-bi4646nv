; Question 2 - Mergesort
(defun partition (lst)
  (labels ((split (l left right)
             (cond
               ((null l) (list left right)) ;; If list is empty, return both halves
               ((null (cdr l)) (list (append left (list (car l))) right)) ;; If there is one element left, put it in left
               (t (split (cddr l) ;; Recursively call split. First two elements will be sorted, the rest will go into the next call
                         (append left (list (car l))) ;; First element goes in left
                         (append right (list (cadr l)))))))) ;; Second goes in right
          (split lst '() '()))) ;; Starts recursion with empty left and right

(defun my-merge (left right)
  (cond
    ((null left) right)  ;; If left is empty, return right
    ((null right) left)  ;; If right is empty, return left
    ((<= (car left) (car right))  ;; If left's first element is smaller, take left's first element and make recursive call
     (cons (car left) (my-merge (cdr left) right))) 
    (t  ;; If right's first element is smaller, take right's first element and make recursive call
     (cons (car right) (my-merge left (cdr right))))))

(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst))) lst ;; If there is 0 or 1 element, return the list
      (let ((halves (partition lst))) ;; Use defined partition function
        (let ((left (mergesort (car halves)))  ;; Recursively sort left half
              (right (mergesort (cadr halves))))  ;; Recursively sort right half
        (my-merge left right)))))  ;; Use my-merge function I defined
