; This function is the main class for the LISP interpreter. This function takes in two
; parameters E and P. E is an expression represented as a string. P is a list of custom
; user-defined functions in the form (fname (param1 param2 ...) = (body)). This function
; only has the functions implemented according to the project requirements.
(defun fl-interp (E P)
    (if (atom E)
        E
        (let*
            ((f (car E))
            (prims '(first if null atom eq cons equal car cdr number + - * > < = and or not))
            (args (cdr E))
            )
            (cond
                ((member f prims) (fl-eval-prims f args P))
                ((find-definition f (len args) P) (fl-eval-custom f args P))
                (t (mapcar (lambda (x) (fl-interp x P)) E))
            )
        )
    )
)

; This function is called whenever the interpreter function fl-interp needs to interpret
; primitive functions. Primitive functions are those listed below. They are required as
; per the project requirements.
(defun fl-eval-prims (f args P) ; f = first
    (cond
        ((equal f 'if) (custom-if args P))
        ((equal f 'null) (apply f (map-args args P)))
        ((equal f 'atom) (apply f (map-args args P)))
        ((equal f 'eq) (apply f (map-args args P)))
        ((equal f 'cons) (apply f (map-args args P)))
        ((equal f 'equal) (apply f (map-args args P)))
        ((equal f 'car) (apply f (map-args args P)))
        ((equal f 'cdr) (apply f (map-args args P)))
        ((equal f 'number) (custom-number args P))
        ((equal f '+) (apply f (map-args args P)))
        ((equal f '-) (apply f (map-args args P)))
        ((equal f '*) (apply f (map-args args P)))
        ((equal f '>) (apply f (map-args args P)))
        ((equal f '<) (apply f (map-args args P)))
        ((equal f '=) (apply f (map-args args P)))
        ((equal f 'and) (custom-and args P))
        ((equal f 'or) (custom-or args P))
        ((equal f 'not) (apply f (map-args args P)))
        (t NIL)
    )
)

; This function defines the implementation for the if statement. This function ensures
; that the if statement is evaluated using applicative order reduction. The function
; takes in a list of arguments through parameter args. This list of arguments must
; be of length 3. The first element in args must be a list containing the condition
; statement. The second element in args must be a list containing the then statement.
; The third element in args must be a list containing the else statement. The P
; variable contains the user-defined functions.
(defun custom-if (args P)
    (let*
        ((condition-var (car args))
        (then-var (cadr args))
        (else-var (caddr args))
        )
        (if (null (fl-interp condition-var P))
            (fl-interp else-var P)
            (fl-interp then-var P)
        )
    )
)

; This function implements the numberp function in the interpreter. This function
; ensure that applicative order reduction is followed. The function takes in a list of
; arguments through the parameter args. This list of arguments must be of length 3. The
; first element in args must be a list containing the condition statement. The second
; element in args must be a list containing the then statement. The third element in
; args must be a list containing the else statement. The P variable contains the
; user-defined functions.
(defun custom-number (args P)
    (let*
        ((x (car args)))
        (numberp (fl-interp x P))
    )
)

; This function implements the AND logic. This function ensures that applicative order
; reduction is followed. The list of arguments are defined in the args parameter. The
; args parameter must only contain 2 elements.  The first element
; is the first condition to be evaluted. The second element is the second condition to
; be evaluated. The P variable contains the user-defined functions.
(defun custom-and (args P)
    (let*
        (
        (left (car args))
        (right (cadr args))
        (ev-left (fl-interp left P))
        (ev-right (fl-interp right P))
        )
        ;; (and (fl-interp left P) (fl-interp right P))
        (if (null ev-left)
            NIL
            (if (null ev-right)
                NIL
                t
            )
        )
    )
)

; This function implements the OR logic. This function ensures that applicative order
; reduction is followed. The list of arguments are passed through the args parameter.
; The first element in args is the first condition to be evaluated. The second element
; is the second condition to be evaluated. The P variable contains the user-defined
; functions.
(defun custom-or (args P)
    (let*
        (
        (left (car args))
        (right (cadr args))
        (ev-left (fl-interp left P))
        (ev-right (fl-interp right P))
        )
        (if (null ev-left)
            (if (null ev-right)
                NIL
                t
            )
            t
        )
    )
)

; This function handles the user-defined functions. This function contains three
; parameters: f, args, and P. The f parameter is the function name of the user-
; defined function. The args parameter is a list containing the arguments to be
; passed to the user-defined function. The P variable contains the function
; definitions for the user-defined functions.
(defun fl-eval-custom (f args P)
    (let*
        ((target (find-definition f (len args) P)))
        (if (null target)
            (list f args)
            (let*
                ((tparams (car target))
                (tbody (cadr target))
                (targs (map-args args P)))
                (fl-interp (substitute-args targs tparams tbody) P)
            )
        )
    )
)

; This function counts the number of elements in the list L. It returns
; a number representing the number of elements in list L.
(defun len (L) 
    (cond
        ((null L) 0)
        (t (+ 1 (len (cdr L))))
    )
)

; This function searches to see if a function is a user-defined function. The
; function checks if a function is a user-defined function by comparing the
; function name fname and the number of arguments arglen with all the function
; definitions in list P. This function returns the list of parameters and the
; body of the user-defined function in P.
(defun find-definition (fname arglen P)
    (let*
        ((tname (caar P))
        (tparams (cadar P))
        (tbody (car (cdddar P))))
        (cond
            ((null P) NIL)
            ((and (equal tname fname) (equal (len tparams) arglen)) (list tparams tbody))
            (t (find-definition fname arglen (cdr P)))
        )
    )
)

; This function substitutes the paramters in the function body with the arguments.
; This function replaces all the parameters param in the function body body with the
; arguments in args. This function returns the modified body with the parameters
; substituted with their arguments.
(defun substitute-args (args params body)
    (cond
        ((null args) body)
        (t (substitute-args (cdr args) (cdr params) (substitute-exp (car params) (car args) body)))
    )
)

; This is a helper function for the substitute-args function. This function replaces each
; occurrence of E1 in list L with E2. The function returns the modified list L with values
; replaced.
(defun substitute-exp (E1 E2 L)
    (cond
        ((null L) NIL)
        ((equal E1 (car L)) (cons E2 (substitute-exp E1 E2 (cdr L))))
        ((not (atom (car L))) (cons (substitute-exp E1 E2 (car L)) (substitute-exp E1 E2 (cdr L))))
        (t (cons (car L) (substitute-exp E1 E2 (cdr L))))
    )
)

; This function calls the fl-interp function on each of the arguments in list args.
(defun map-args (args P)
    (mapcar (lambda (x) (fl-interp x P)) args)
)

;; Testing user defined functions
(print (fl-interp '(f (f 2)) 
'( (f (X) = (* X X)) )))

(print (fl-interp '(a (+ 1 2)) 
'( (a (X) = (+ X 1)) )))

(print (fl-interp '(b (+ 1 2)) 
'( (b (X) = (+ X 1)) )))

;; (print (fl-interp '(h (g 5))
;;  '( (g (X) = (g (g X)))
;;  (h (X) = 1 ) )))

;; (set 'E '(reverse (1 2 3)))
;; (set 'P '((reverse (X) = (if (null X) nil (append (reverse (car X)) (cons (car X) nil)))) (append (X Y) = (if (null X) Y (cons (car X) (append (cdr X) Y))))))
;; (print (fl-interp '(xmap number (1 A 2 B 3)) '((xmap (f L) = (if (null L) nil (cons (f (car L)) (xmap f (cdr L)))))))) ; (t nil t nil t)
;; (print (fl-interp '(xmap is_positive (1 -2 3)) '((xmap (f L) = (if (null L) nil (cons (f (car L)) (xmap f (cdr L))))) (plus1 (X) = (+ X 1)) (is_positive (X) = (if (> X 0) t nil))))) ; (t nil t)
;; (print (fl-interp '(xmap plus1 (1 2 3)) '((xmap (f L) = (if (null L) nil (cons (f (car L)) (xmap f (cdr L))))) (plus1 (X) = (+ X 1)) (is_positive (X) = (if (> X 0) t nil))))) ; (2 3 4)
;; (print (fl-interp '(count (1 2 3 4)) '((count (L) = (if  (null L) 0 (+ 1 (count (cdr L))))))))

;; (print (fl-interp '(+ 1 2) nil))

;; (print (fl-interp '(insert (insert NIL 6) 5)
;;     '((insert (Tr Int) =
;;         (if (null Tr)
;;             (cons NIL (cons INT (cons NIL NIL)))
;;             (if (equal (nodeValue Tr))
;;                 "Node already in"
;;                 (if (< Int (nodeValue Tr))
;;                     (list (insert (leftTree Tr) Int) (nodeValue Tr) (rightTree Tr))
;;                     (list (leftTree Tr) (nodeValue Tr) (insert (rightTree Tr) Int))
;;                 )
;;             )
;;         )
;;     )
;;     (nodeValue (Tr) = (if (null Tr)
;;         NIL
;;         (car (cdr Tr))
;;     )) 
;;     (leftTree (Tr) = (if (null Tr)
;;         NIL
;;         (car Tr)
;;     ))
;;     (rightTree (Tr) = (if (null Tr)
;;         NIL
;;         (car (cdr (cdr Tr)))
;;     ))
;; )))