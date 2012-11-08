(defun heuristic (node)
	   (if (unitp  (node-depth node))
	       (node-path-cost node)
	       (+ 1 (node-path-cost node))))

(defun make-heap (&optional(size 100000))
	     (make-array size :fill-pointer 0 :adjustable t))

(defun heap-val (heap i key) (funcall key (elt heap i)))

(defun heap-parent (i) (floor (1- i) 2))

(defun heap-left(i) (+ 1 i i))

(defun heap-right(i) (+ 2 i i))

(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heap-find-pos (heap i val key)
	   (cond ((or (zerop i) (< (heap-val heap (heap-parent i) key) val))
		  i)
		 (t (setf (elt heap i) (elt heap (heap-parent i)))
		    (heap-find-pos heap (heap-parent i) val key))
		 ))
	       

(defun heap-insert(heap item key)
	   (vector-push-extend nil heap)
	   (setf (elt heap (heap-find-pos heap (1- (length heap)) (funcall key item) key))
		 item)
	   )

(defun heapify (heap i key)
	   (unless (heap-leafp heap i)
	     (let ((left-index (heap-left i))
		   (right-index (heap-right i)))
	       (let ((smaller-index
		      (if (and (< right-index (length heap))
			       (< (heap-val heap right-index key)
				  (heap-val heap left-index key)))
			  right-index
			  left-index)))
		 (when (> (heap-val heap i key)
			  (heap-val heap smaller-index key))
		   (rotatef (elt heap i)
			    (elt heap smaller-index))
		   (heapify heap smaller-index key))))
	     ))

(defun heap-pop (heap key)
	   (let ((min (elt heap 0)))
	     (setf (elt heap 0) (elt heap (1- (length heap))))
	     (decf (fill-pointer heap))
	     (heapify heap 0 key)
	     min))

(defstruct q
  (enqueue #'enqueue)
  (key #'heuristic)
  (last nil)
  (elements nil))

(defun q-emptyp(q)
	   (= (length (q-elements q)) 0))

(defun q-remove (q)
  "Removes and returns the element at the front of the queue."
  (if (listp (q-elements q))
      (pop (q-elements q))             
    (heap-pop (q-elements q) (q-key q))))


(defun q-insert (q items)
	   (funcall (q-enqueue q) q items)
	   q)

(defun make-node (&key state (parent nil)
			(action nil)
			(path-cost 0) (depth nil))
	(list state parent action path-cost depth))

(defun node-state (node) (car node))
(defun node-parent (node) (cadr node))
(defun node-action (node) (caddr node))
(defun node-path-cost (node) (cadddr node))
(defun node-depth (node) (car (cddddr node)))

(defstruct node 
 state (parent nil) (action nil) (path-cost 0) (depth nil))

(defun enqueue(q newnode)
	   (when (null (q-elements q))
	     (setf (q-elements q)(make-heap)))
	   (mapc (lambda (item)
		   (heap-insert (q-elements q) item (q-key q)))
		 newnode)
	   newnode)

(defun varp (x)
	   (cond((and (atom x) (eql '#\? (elt (symbol-name x) 0))) T)
		((and (listp x) (not (equal (length x) 1)) (symbolp (car x)) (eql '#\@ (elt (symbol-name (car x)) 0))) T)
		(t nil)))

(defun occurs-p (var x theta)
	   (cond ((eql var x) t)
		 ((and (listp var) (not (equal (length var) 1)) (eql '#\@ (elt (symbol-name (car var)) 0)) (or (eql (car (cdr var))  x) (member x (cdr var)))) t)
		 ((and (varp x) (assoc x theta :test #'equal))
		  (occurs-p var (cdr (assoc x theta :test #'equal)) theta))
		 ((consp x) (or (occurs-p var (car x) theta)
				(occurs-p var (cdr x) theta)))
		 (t nil)))

(defun unify-var (var x theta)
	   (let ((vb (assoc var theta :test #'equal))
		 (xb (assoc x theta :test #'equal)))
	     (cond (vb (unify (cdr vb) x theta))
		   (xb (unify var (cdr x) theta))
		   ((occurs-p var x theta) 'fail)
		   (t (cons (cons var x) theta)))))

(defun unify (x y &optional (theta nil))
	   (cond ((eql theta 'fail) 'fail)
		 ((eql x y) theta)
		 ((varp x) (unify-var x y theta))
		 ((varp y) (unify-var y x theta))
		 ((and (consp x) (consp y))
		  (unify (cdr x) (cdr y)(unify (car x) (car y) theta)))
		 (t 'fail)))

(defun unitp (clause)
	   (if (not (eql '_OR (car clause)))
	       T
	       NIL))

(defun modify2 (clause)
	   (if (and (> (length clause) 1) (not (equal (cdr clause) nil)) (listp (car clause)) (listp (second clause)))
                 (cons '_OR clause)
		 clause))

(defun modify (clause)
	   (cond((and (> (length clause) 1) (not (equal (cdr clause) nil)) (listp (car clause))(listp (second clause))) (cons '_OR clause))
		((equal (length clause) 1) (car clause))
		(t clause)))


(defun remodify (clause)
           (cond((equal (car clause) '_OR) (cdr clause))
		(t (list clause))))


(defun unifiable (clause1 clause2)
	   (cond((and (equal '_NOT (car clause1)) (not(equal '_NOT (car clause2))) (equal (car (cadr clause1)) (car clause2)) (not (equal 'fail (unify  (cdr (cadr clause1)) (cdr clause2)))) (not (equal nil (unify  (cdr (cadr clause1)) (cdr clause2))))) (unify  (cdr (cadr clause1)) (cdr clause2)))
		((and (equal '_NOT (car clause2)) (not(equal '_NOT (car clause1))) (equal (car (cadr clause2)) (car clause1)) (not (equal 'fail (unify  (cdr (cadr clause2)) (cdr clause1)))) (not (equal nil (unify  (cdr (cadr clause2)) (cdr clause1))))) (unify (cdr clause1) (cdr (cadr clause2)) ))
		((and (equal '_NOT (car clause1)) (not(equal '_NOT (car clause2))) (equal (car (cadr clause1)) (car clause2)) (equal nil (unify  (cdr (cadr clause1)) (cdr clause2)))) 'empty)
		((and (equal '_NOT (car clause2)) (not(equal '_NOT (car clause1))) (equal (car (cadr clause2)) (car clause1)) (equal nil (unify  (cdr clause1) (cdr (cadr clause2))))) 'empty)
		(t nil)))


(defun resolve (clause1 clause2)
	   (cond((and (unitp clause1) (equal (length clause1) 1)) (resolve (car clause1) clause2))
		((and (unitp clause2) (equal (length clause2) 1)) (resolve clause1  (car clause2)))
	        ((and (unitp clause1) (unitp clause2) (not (equal 'empty (unifiable clause1 clause2)))(unifiable clause1 clause2))  (list (list clause1 clause2) (unifiable clause1 clause2)))
		((and (unitp clause1) (unitp clause2) (equal 'empty (unifiable clause1 clause2))) (list (list clause1 clause2) '(nil)))
		((and (unitp clause1) (not (unitp clause2))) (if (unifiable clause1 (car (cdr clause2)))
								 (resolve clause1 (car (cdr clause2)))
								 (resolve clause1 (modify2 (cdr(cdr clause2))))))
		((and (unitp clause2) (not (unitp clause1))) (resolve clause2 clause1))
		((and (not (unitp clause1)) (not (unitp clause2))) (if (resolve (car (cdr clause1)) clause2)
								       (resolve (car (cdr clause1)) clause2)
								       (resolve (modify2 (cdr (cdr clause1))) clause2)))
		(t nil)))

(defun combine (clause1 clause2)
	   (cond((and (unitp clause1)(not (listp (car clause1)))) (combine (list clause1) clause2))
		((and (unitp clause2)(not (listp (car clause2)))) (combine clause1 (list clause2)))
	        ((equal (car clause1) '_OR) (combine (cdr clause1) clause2))
		((equal (car clause2) '_OR) (combine clause1 (cdr clause2)))
		(t (append clause1 clause2))))

(defun removepair (clause1 clause2)
	   (remove (second clause2) (remove (car clause2) clause1 :test #'equal) :test #'equal))

(defun subs (eachclause eachtheta)
	   (cond((equal (car eachclause) '_NOT) (list '_NOT (subs (second eachclause) eachtheta)))
		(t (substitute (cdr eachtheta)(car eachtheta) eachclause :test #'equal))))

(defun substheta (eachclause theta)
	   (cond ((equal (length theta) 1) (subs eachclause (car theta)))
		 (t (substheta (subs eachclause (car theta)) (cdr theta)))))

(defun subsclause (clause theta)
	   (mapcar (lambda (eachclause)
		     (substheta eachclause theta))
		   clause))

(defun resolution (clause1 clause2)
	   (if(equal (resolve clause1 clause2) nil)
	      (remove-duplicates (combine clause1 clause2) :test #'equal )
	      (let ((pair (car (resolve clause1 clause2)))
		 (theta (car (cdr (resolve clause1 clause2))))
		 (comb (combine clause1 clause2)))		  
	     (remove-duplicates (subsclause (removepair comb pair) theta) :test #'equal))))


(defun samep (node1 node2)
	   (cond ((and node1 node2 (node-parent node1) (node-parent node2) (equal (node-state (node-parent node1)) (node-state (node-parent node2))) (equal (node-depth node1) (node-depth node2))) T)
		 ((and node1 node2 (node-parent node1) (node-parent node2)(equal (node-state (node-parent node1)) (node-depth node2)) (equal (node-depth node1) (node-state (node-parent node2)))) T)
		 (t nil)))


(defun heuristic (node)
	   (if (unitp  (node-depth node))
	       (node-path-cost node)
	       (+ 1 (node-path-cost node))))


(defparameter s-state NIL)
(defparameter s-action NIL)
(defparameter s-cost NIL)
(defparameter s-depth NIL)

(defun successor (node)
 (remove NIL (mapcar (lambda (each-action)
                       (setq s-state (modify (resolution (node-state node) each-action)))
		       (setq s-action (node-action node))
		       (setq s-cost 0)
		       (setq s-depth each-action)   

		       (cond ((equal (resolve (node-state node) each-action)  nil) NIL) 
			     ((equal (length (resolution (node-state node) each-action)) (+ (length (remodify each-action)) (length (remodify (node-state node))))) NIL)
			     (t (list s-action s-state s-cost s-depth)))
		       )
		       
		     (node-action node))))

(defun expand(successor node)
	   (let ((triples (funcall successor node)))
	     (mapcar (lambda (action-state-cost)
		       (let ((action (car action-state-cost))
			     (state (cadr action-state-cost))
			     (cost (caddr action-state-cost))
			     (depth (cadddr action-state-cost)))
			 (make-node :state state
				    :parent node
				    :action action
				    :path-cost (+ (node-path-cost node) cost)
				    :depth depth)
		       ))
	     triples)
	     ))


(defun action-sequence (node &optional (actions nil))
	   (if (node-parent node)
	       (action-sequence (node-parent node)
				(cons (list (remodify (node-state (node-parent node))) (remodify (node-depth node))  (node-state node) (car (cdr (resolve (node-state (node-parent node)) (node-depth node)))))  actions))
	       actions))

(defun goalp (node)
	   (if (and (node-parent node) (equal nil (resolution (node-state (node-parent node)) (node-depth node))))
	       T
	       NIL))


(defun graph-search (fringe closed successor goalp samep)
	   (unless (q-emptyp fringe)
	     (let((node (q-remove fringe)))
	     (cond ((funcall goalp node)
		    (action-sequence node))
		   ((member node closed :test #'samep)
		    (graph-search fringe closed successor goalp samep))
		   (t (let ((successors (expand successor node)))
			(graph-search (q-insert fringe successors)
				      (cons node closed)
				      successor goalp samep)
			
			))
		 ))))

(defun general-search (initial-KB initial-state successor goalp &key (samep #'samep) (enqueue #'enqueue) (key #'heuristic))
	   (let ((fringe (make-q :enqueue enqueue :key key)))
	     (q-insert fringe (list (make-node :state initial-state :action (cdr initial-KB))))
	     (graph-search fringe nil successor goalp samep)))

(defun atp (kb nq)
	   ( general-search kb (elt nq 1) #'successor #'goalp :key #'heuristic))


(defun implication-p (expr)
  (and (consp expr)
       (or (eql (first expr) '=>) (eql (first expr) '<=) (eql (first expr) '<=>))))

(defun negated-p (expr)
  (and (consp expr)
       (eql (first expr) '_NOT)))

(defun conj-p (expr)
  (and (consp expr)
       (eql (first expr) '_AND)))

(defun disj-p (expr)
  (and (consp expr)
       (eql (first expr) '_OR)))

(defun exists-p (expr)
  (and (consp expr)
       (eql (first expr) '_EXI)))

(defun forall-p (expr)
  (and (consp expr)
       (eql (first expr) '_ALL)))



(defun replace-fol-implications1 (expr)
  (if (consp expr)
      (if (eql (first expr) '=>)
	  (list '_OR
		(list '_NOT (replace-fol-implications1 (second expr)))
		(replace-fol-implications1 (third expr)))
	(loop for el in expr
	    collect
	      (replace-fol-implications1 el)))
    expr))


(defun replace-fol-implications2 (expr)
  (if (consp expr)
      (if (eql (first expr) '<=)
	  (list '_OR
		(replace-fol-implications2 (second expr))
		(list '_NOT (replace-fol-implications2 (third expr))))
		
	(loop for el in expr
	    collect
	      (replace-fol-implications2 el)))
    expr))


(defun replace-fol-implications3 (expr)
  (if (consp expr)
      (if (eql (first expr) '<=>)
	  (list '_AND
	      (list '_OR
		   (list '_NOT (replace-fol-implications3 (second expr)))
		   (replace-fol-implications3 (third expr)))
	      (list '_OR
		   (replace-fol-implications3 (second expr))
		   (list '_NOT (replace-fol-implications3 (third expr)))))
		
	(loop for el in expr
	    collect
	      (replace-fol-implications3 el)))
    expr))

(defun move-negation-in (expr neg-p)
   (cond ((forall-p expr) (list (if neg-p '_EXI '_ALL)
			       (second expr)
			       (move-negation-in (third expr) neg-p)))
	((exists-p expr) (list (if neg-p '_ALL  '_EXI)
			       (second expr)
			       (move-negation-in (third expr) neg-p)))
	((conj-p expr) (list (if neg-p '_OR '_AND)
			     (move-negation-in (second expr) neg-p)
			     (move-negation-in (third expr) neg-p)))
	((disj-p expr) (list (if neg-p '_AND '_OR)
			     (move-negation-in (second expr) neg-p)
			     (move-negation-in (third expr) neg-p)))
        ((negated-p expr) (move-negation-in (second expr) (not neg-p)))
	(t (if neg-p 
	       (list '_NOT expr)
	       expr))))


(defvar *var-to-assign* -1)

(defun next-var()
 (setq *var-to-assign* (1+ *var-to-assign*))
 (nth *var-to-assign* '(?x ?x ?y ?y ?z ?z ?w ?w ?v ?v ?a ?a ?b ?b ?c ?c ?d ?d ?e ?e ?f ?f ?g ?g ?h ?h ?i ?i ?j ?j ?k ?k ?l ?l ?m ?m ?n ?n ?o ?o ?p ?p ?q ?q ?r ?r ?s ?s ?t ?t ?u ?u ?x ?x ?y ?y ?z ?z ?w ?w ?v ?v ?a ?a ?b ?b ?c ?c ?d ?d ?e ?e ?f ?f ?g ?g ?h ?h ?i ?i ?j ?j ?k ?k ?l ?l ?m ?m ?n ?n ?o ?o ?p ?p ?q ?q ?r ?r ?s ?s ?t ?t ?u ?u ?x ?x ?y ?y ?z ?z ?w ?w ?v ?v ?a ?a ?b ?b ?c ?c ?d ?d ?e ?e ?f ?f ?g ?g ?h ?h ?i ?i ?j ?j ?k ?k ?l ?l ?m ?m ?n ?n ?o ?o ?p ?p ?q ?q ?r ?r ?s ?s ?t ?t ?u ?u ?x ?x ?y ?y ?z ?z ?w ?w ?v ?v ?a ?a ?b ?b ?c ?c ?d ?d ?e ?e ?f ?f ?g ?g ?h ?h ?i ?i ?j ?j ?k ?k ?l ?l ?m ?m ?n ?n ?o ?o ?p ?p ?q ?q ?r ?r ?s ?s ?t ?t ?u ?u))
)


(defun stand-var (clause)
	   (if (consp clause)
	       (cond((eql '_ALL (first clause))
		(list '_ALL (next-var) (replace-variable (next-var) (second clause) (stand-var (car (nthcdr 2 clause))))))
		     ((eql '_EXI (first clause))
		      (list '_EXI (next-var) (replace-variable (next-var) (second clause) (stand-var (car (nthcdr 2 clause))))))
		     ((eql '_AND (first clause))
		      (cons '_AND (stand-var (rest clause))))
		    (t (loop for el in clause
		       collect
		       (stand-var el))))
	       clause))

(defun replace-variable ( new old expr)
	   (if(consp expr)
	      (if (and (not (negated-p  expr)) (not (conj-p expr)) (not (disj-p  expr)) (not (exists-p expr)) (not (forall-p  expr)))
		  (substitute new old  expr :test #'equal)
 		  
	      (loop for el in expr
	       collect
	      (replace-variable new old el)))
	      expr))


(defvar *mystack* nil)

(defun move-quantifiers (expr)
  (setf *mystack* nil)
  (let ((qfree (move-quantifiers-aux expr)))
    (replace-quantifiers *mystack* qfree)))

(defun move-quantifiers-aux (expr)  
  (cond ((not (consp expr)) expr)
        ((or (exists-p expr) (forall-p expr))
         (push (list (first expr) (second expr)) *mystack*)
	 (move-quantifiers-aux (third expr)))
	(t (loop for el in expr
	       collect
	       (move-quantifiers-aux el)))))

(defun replace-quantifiers (mystack expr)
  (if mystack (replace-quantifiers (rest mystack)
				  (append (first mystack)
					  (list expr)))
    expr))

(defun skolemize (expr proper-p univ-store)
  (cond ((exists-p expr)
	 (let* ((bv (second expr))
		(skolem (create-skolem bv univ-store proper-p)))
	   (skolemize (subst skolem bv (third expr))
		           proper-p univ-store)))
	((forall-p expr)
	 (list (first expr) 
	       (second expr)
	       (skolemize (third expr) proper-p 
			  (cons (second expr) univ-store))))
	(t expr)))

(defun create-skolem (var univ-store proper-p)
  (if (and proper-p univ-store)
      (cons '@f 
            univ-store)
      (intern (concatenate 'string "CONSTANT"(subseq (string var) 1)))))

(defun drop-universals (expr)
  (if (forall-p expr)
      (drop-universals (third expr))
      expr))

(defun distribute (expr)
  (cond ((disj-p expr) 
	 (let ((subexpr1 (distribute (second expr)))
	       (subexpr2 (distribute (third expr))))
	   (if (conj-p subexpr1)
	       (if (conj-p subexpr2) 
		   (cons '_AND
			 (loop for subconj1 in (rest subexpr1)
			     append
			       (loop for subconj2 in (rest subexpr2)
				   collect
				     (distribute-aux1 subconj1 subconj2))))		 
		 (cons '_AND
		       (loop for subconj in (rest subexpr1)
			   collect
			     (distribute-aux1 subconj subexpr2))))
	        (if (conj-p subexpr2)		 
		 (cons '_AND
		       (loop for subconj in (rest subexpr2)
			   collect
			     (distribute-aux1 subexpr1 subconj)))
	       
	       (distribute-aux1 subexpr1 subexpr2)))))
	((conj-p expr)
	 (cons '_AND
	       (loop for subconj in (rest expr)
		   append
		     (let ((new-expr
			    (distribute subconj)))
		       (if (conj-p new-expr)
			   (rest new-expr) 
			   (list new-expr))))))
	(t expr)))

(defun distribute-aux1 (sub1 sub2)
  (cons '_OR
	(append
	 (distribute-aux2 sub1) 
	 (distribute-aux2 sub2))))
		     
(defun distribute-aux2 (subexpr)
  (if (disj-p subexpr)
      (rest subexpr) 
    (list subexpr)))

;with standarized variable 
(defun convert-fol-to-snf (fol-exp &key (proper-skolem t))
  (let* ((step1 (replace-fol-implications1 fol-exp))
	 (step2 (replace-fol-implications2 step1))
	 (step3 (replace-fol-implications3 step2))
	 (step4 (move-negation-in step3 nil))
	 (step4_1 (stand-var step4))
	 (step5 (move-quantifiers step4_1))
	 (step6 (skolemize step5 proper-skolem nil))
         (step7 (drop-universals step6))
	 (step8 (distribute step7)))
    step8))

;without standarized variable 
;(defun convert-fol-to-snf (fol-exp &key (proper-skolem t))
; (let* ((step1 (replace-fol-implications1 fol-exp))
;	 (step2 (replace-fol-implications2 step1))
;	 (step3 (replace-fol-implications3 step2))
;	 (step4 (move-negation-in step3 nil))
;	 (step5 (move-quantifiers step4))
;	 (step6 (skolemize step5 proper-skolem nil))
;         (step7 (drop-universals step6))
;	 (step8 (distribute step7)))
;    step8))

(defun andp (clause)
	   (eql '_AND (car clause)))

(defun removeand (clause)
	   (if (eql (car clause) '_AND)
	       (rest clause)
	       clause))

(defun process (clauses)
	   (cond ((and (listp (car clauses)) (listp (car (cdr clauses)))(andp (car clauses)) )
	       (append (removeand (car clauses)) (process (cdr clauses))))
		 ((and (listp (car clauses)) (eql nil (cdr clauses))) (list (removeand (car clauses))))
		 ((and (listp (car clauses)) (listp (car (cdr clauses)))) (cons (car clauses) (process (cdr clauses))))
	       (t clauses)))


(defun fol (clauses)
	   (cond((and (eql '_AND (first clauses)) (eql (length (rest clauses)) 1)) (list '_AND (process (convert-fol-to-snf (car (cdr clauses))))))
		((and (eql '_AND (first clauses)) (> (length (rest clauses)) 1)) (cons '_AND (process (mapcar #'convert-fol-to-snf (rest clauses)))))
		((not(eql '_AND (first clauses))) (list '_AND  (convert-fol-to-snf clauses)))
		))

(defun atp-fol (kb2 nq2)
	   (let ((fkb-fol (fol kb2))
		 (fnq-fol (fol nq2)))
	     (atp fkb-fol fnq-fol)))