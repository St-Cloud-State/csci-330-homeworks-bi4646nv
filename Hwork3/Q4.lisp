; Question 4 - Insertion Sort
(defun insert (temp sorted)
  (cond
    ((null sorted) (list temp))  ;; If sorted is empty, create a new list with temp
    ((<= temp (car sorted)) (cons temp sorted))  ;; If temp is smaller than the first element, place it at the front
    (t (cons (car sorted) (insert temp (cdr sorted))))))  ;; Otherwise, insert temp will be called recursively until the correct position is found

(defun insertion-sort-helper (sorted unsorted) ;; Once again, helper is used 
  (if (null unsorted) sorted ;; If unsorted is empty, the sorted list is returned
      (insertion-sort-helper  ;; Make recursive calls to insert elements into sorted
        (insert (car unsorted) sorted)  ;; Insert the first element of unsorted into sorted
        (cdr unsorted))))  ;; Process the remaining unsorted elements

(defun insertion-sort (lst)
  (insertion-sort-helper '() lst))  ;; Start sorting with an empty "sorted" list