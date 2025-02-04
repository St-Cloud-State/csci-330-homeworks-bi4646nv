(defun MILES-PER-GALLON (INITIAL-ODOMETER-READING FINAL-ODOMETER-READING GALLONS-CONSUMED)
    (/ (- FINAL-ODOMETER-READING INITIAL-ODOMETER-READING) GALLONS-CONSUMED))

(defun mystery (x)
    (list (second x) (first x)))

(defun speak (x y)
    (list 'all x 'is y))