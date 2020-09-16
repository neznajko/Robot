;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;_
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst pi   (* 2 (acos 0)) "pi")
(defconst _2pi (* 2 pi)       "2* pi")
(defvar stk () "Solution Stack")
(defvar fpnt () "First Point")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dtor (d)
  "Degrees to radians."
  (* pi (/ d 180.0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rtod (r)
  "Radians to degrees."
  (* 180.0 (/ r pi)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun phi (p q)
  "Return the polar angle of the vector
defined by p and q points in the range
[0, 2* pi)."
  (let* ((x1  (car p))
         (y1  (car (cdr p)))
         (x2  (car q))
         (y2  (car (cdr q)))
         (x   (- x2 x1))
         (y   (- y2 y1))
         (phi (atan y x)))
    (if (< phi 0)
        (+ phi _2pi)
      phi)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ngle (ui uf vi vf)
  "Oriented angle between u and v,
(-pi, pi]"
  (let* ((a (phi ui uf))
		 (b (phi vi vf))
		 (c (- b a)))
	(if (> c pi)
		(- c _2pi)
	  c)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cons-point (str)
  "Construct point from string."
  (let ((bf (split-string str)))
    (mapcar 'string-to-number bf)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load (file-name)
  "Load nput file as list of points."
  (let* ((bf (with-temp-buffer
               (insert-file file-name)
               (buffer-string)))
         (ls (split-string bf "\n" t)))
    (mapcar 'cons-point (cdr ls))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -ryth-ngle- (x)
  "Right angle ck."
  (setq x (abs (rtod x)))
  (or (= x 90) (= x 270)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -cycle- (V)
  "First point cycle check."
  (let* ((b (car V))
         (a (car (cdr V)))
         (c fpnt)
         (n (ngle a b b c)))
    (-ryth-ngle- n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _0xFF01 (c p U V)
  "Search for solutions:
   c - current point
   p - previous point
   U - unvisited points
   V - visited points"
  ;; ck if U is empty
  (if (not U)
      ;; now V is full, ck for cycle
      (if (-cycle- V) (push V stk))
    ;; loop over U and make ryth ngle ck
    (let ((U1) (V1)) ;; local variables
      (dolist (n U)
        (if (-ryth-ngle- (ngle p c c n))
            (progn
              (setq V1 (cons n V)
                    U1 (remove n U))
              (_0xFF01 n c U1 V1)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun robot ()
  "Main function."
  (let ((bf (load "nput"))
        (U) (V))
    (setq fpnt (pop bf))
    (dolist (c bf)
      (setq V (cons c nil)
			U (remove c bf))
      (_0xFF01 c fpnt U V))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pinst (inst p)
  "Point Instructon"
  (let ((x (car p))
		(y (car (cdr p))))
	(format "%s %d %d\n" inst x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun program ()
  "Convert first stk route to program."
  (switch-to-buffer "program")
  (let* ((V (car stk))
		 (f fpnt)
		 (k (length V))
		 (V (cons f (append V (list f))))
		 (i 0)
		 (n) (a) (b) (c))
	(insert (pinst "ORIENTATION" 
				   (car (cdr V))))
	(while (< i k)
	  (setq a (nth (+ 0 i) V)
			b (nth (+ 1 i) V)
			c (nth (+ 2 i) V)
			n (ngle a b b c))
	  (if (< n 0) (insert "TURN-RIGHT\n")
		(insert "TURN-LEFT\n"))
	  (insert (pinst "MOVE-TO" c))
	  (cl-incf i)))
  (insert "STOP\n"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; log:
; . . . . . . . . . . . . . . . . .  8
; . . . . . . . . . g . . . . . . .
; . . . . . . . . . . . . f . . . .  6
; . . . . . c . . . . . . . . . . .
; . . . . . . . . b . . . . . . . .  4
; . . . . . . . . . . . e . . . . . 
; . . . . . . . . . . . . . . d . .  2
; . . . . . . . . . . . . . . . . . 
; . . . . . . . . x . . . . . . . .  0
; . . . h . . . . . . . . . . . . .
; . . . . . . . . . . . . . . . . . -2
; . . . . . . . . . . . . . . . . .
; . . . . . . . . . . . . a . . . . -4
; . . . . . . . . . . . . . . . . .
; . . . . . . . . . . . . . . . . . -6
; . . . . . . . . . . . . . . . . .
; . . . . . . . . . . . . . . . . . -8
;-8  -6  -4  -2   0   2   4   6   8
(robot)
(program)
