; This function inserts a new node in a binary tree. The tree follows the format
; (left root right) where left and right are the tree branches represented as
; nested lists. Root is the root of the tree/branch and it is simply an integer.
; This function only adds unique nodes to the tree. Duplicates will not be added
; to the tree.
(defun insert (Tr Int)
    (cond
        ((null Tr) (cons NIL (cons INT (cons NIL NIL))))
        ((equal (nodeValue Tr) Int) (print "Node already in"))
        ((< Int (nodeValue Tr)) (list (insert (leftTree Tr) Int) (nodeValue Tr) (rightTree Tr)))
        (t (list (leftTree Tr) (nodeValue Tr) (insert (rightTree Tr) Int)))
    )
)

; This function deletes the node Int from the tree Tr. The function ensures that
; the delete preserves the binary tree structure in which the left subtree is
; less than the root and the right subtree is greater than the root.
(defun xdelete (Tr Int)
    (cond
        ((null Int) Tr)
        ((null Tr) NIL)
        ((= Int (nodeValue Tr)) (xdelete-helper Tr))
        ((< Int (nodeValue Tr)) (list (xdelete (leftTree Tr) Int) (nodeValue Tr) (rightTree Tr)))
        (t (list (leftTree Tr) (nodeValue Tr) (xdelete (rightTree Tr) Int)))
    )
)

; This is the helper function for xDelete. This function handles the delete and
; ensures that the binary tree property is maintained. The binary tree property
; that this function maintains is that the left subtree is less than the root.
; The right subtree is greater than the root.
(defun xdelete-helper (Tr)
    (cond
        ((null (leftTree Tr)) (rightTree Tr))
        ((null (rightTree Tr)) (leftTree Tr))
        (t (replace-node Tr (find-min (rightTree Tr)) (xdelete (rightTree Tr) (find-min (rightTree Tr)))))
    )
)

; This function replaces the root node with a new node. This is an additional
; helper function for the xDelete function. When the node gets deleted the
; function replaces the root with the smallest node in the right subtree.
(defun replace-node (Tr NewNode SubTree)
    (list (leftTree Tr) NewNode subTree)
)

; This function finds the minimum value within the tree Tr. This function works
; by continually recursing to the left of the tree.
(defun find-min (Tr)
    (if (null (leftTree Tr))
        (nodeValue Tr)
        (find-min (leftTree Tr))
    )
)

; This function retrieves the value of the root of tree Tr.
(defun nodeValue (Tr)
    (if (null Tr)
        NIL
        (cadr Tr)
    )
)

; This function retrieves the left subtree of the tree.
(defun leftTree(Tr)
    (if (null Tr)
        NIL
        (car Tr)
    )
)

; This function retrieves the right subtree of the tree.
(defun rightTree(Tr)
    (if (null Tr)
        NIL
        (caddr Tr)
    )
)

;; (print (nodeValue '((NIL 2 NIL) 1 NIL)))
;; (print (insert '(NIL 5 (NIL 6 (NIL 7 NIL))) 4))
(print (insert (insert NIL 6) 5))
(print (xdelete '(NIL 5 (NIL 6 (NIL 7 NIL))) 6))
(print (xdelete '(((NIL 1 NIL) 2 (NIL 3 NIL)) 4 ((NIL 5 NIL) 6 (NIL 7 NIL))) 4))
(print (xdelete '(NIL 5 ((NIL 6 NIL) 7 (NIL 8 NIL))) 5))
(print (xdelete '(((NIL 1 NIL) 2 (NIL 3 NIL)) 4 NIL) 4))
(print (xdelete '(((NIL 1 NIL) 2 (NIL 3 NIL)) 4 ((NIL 5 NIL) 6 (NIL 7 NIL))) 2))
(print (xdelete '(((NIL 1 NIL) 2 (NIL 3 NIL)) 4 ((NIL 5 NIL) 6 (NIL 7 NIL))) 6))
(print (xdelete '((NIL 2 (NIL 3 NIL)) 4 ((NIL 5 NIL) 6 (NIL 7 NIL))) 2))
(print (xdelete '(((NIL 1 NIL) 2 NIL) 4 ((NIL 5 NIL) 6 (NIL 7 NIL))) 2))
(print (xdelete '((NIL 2 NIL) 4 ((NIL 5 NIL) 6 (NIL 7 NIL))) 2))
(print (xdelete '(((NIL 1 NIL) 2 (NIL 3 NIL)) 4 (NIL 6 (NIL 7 NIL))) 6))
(print (xdelete '(((NIL 1 NIL) 2 (NIL 3 NIL)) 4 ((NIL 5 NIL) 6 NIL)) 6))
(print (xdelete '(((NIL 1 NIL) 2 (NIL 3 NIL)) 4 (NIL 6 NIL)) 6))
(print (xdelete '(((NIL 1 NIL) 2 (NIL 3 NIL)) 4 ((NIL 5 NIL) 6 (NIL 7 NIL))) 8))
(print (xdelete NIL 1))
(print (xdelete '(((NIL 1 NIL) 2 (NIL 3 NIL)) 4 ((NIL 5 NIL) 6 (NIL 7 NIL))) NIL))
(print (xdelete NIL NIL))
(print (xdelete '((NIL 1 NIL) 2 (NIL 3 (NIL 4 (NIL 5 NIL)))) 2))