

#|
PART 1: encode graph using property lists, each of which should consist 
of 3 items: state, action that leads to state, and action cost.
|#

; (setf (get 'island 'adj) '())
(setf (get 'brest 'adj) '((rennes va-rennes 244)))
(setf (get 'rennes 'adj) '((caen va-caen 176) (paris va-paris 348) (brest va-brest 244) (nantes va-nantes 107)))
(setf (get 'caen 'adj) '((calais va-calais 120) (paris va-paris 241) (rennes va-rennes 176)))
(setf (get 'calais 'adj) '((nancy va-nancy 534) (paris va-paris 297) (caen va-caen 120)))
(setf (get 'nancy 'adj) '((strasbourg va-strasbourg 145) (dijon va-dijon 201) (paris va-paris 372) (calais va-calais 534)))
(setf (get 'strasbourg 'adj) '((dijon va-dijon 335) (nancy va-nancy 145)))
(setf (get 'dijon 'adj) '((strasbourg va-strasbourg 335) (lyon va-lyon 192) (paris va-paris 313) (nancy va-nancy 201)))
(setf (get 'lyon 'adj) '((grenoble va-grenoble 104) (avignon va-avignon 216) (limoges va-limoges 389) (dijon va-dijon 192)))
(setf (get 'grenoble 'adj) '((avignon va-avignon 227) (lyon va-lyon 104)))
(setf (get 'avignon 'adj) '((grenoble va-grenoble 227) (marseille va-marseille 99) (montpellier va-montpellier 121) (lyon va-lyon 216)))
(setf (get 'marseille 'adj) '((nice va-nice 188) (avignon va-avignon 99)))
(setf (get 'nice 'adj) '((marseille va-marseille 188)))
(setf (get 'montpellier 'adj) '((avignon va-avignon 121) (toulouse va-toulouse 240)))
(setf (get 'toulouse 'adj) '((montpellier va-montpellier 240) (bordeaux va-bordeaux 253) (limoges va-limoges 313)))
(setf (get 'bordeaux 'adj) '((limoges va-limoges 220) (toulouse va-toulouse 253) (nantes va-nantes 329)))
(setf (get 'limoges 'adj) '((lyon va-lyon 389) (toulouse va-toulouse 313) (bordeaux va-bordeaux 220) (nantes va-nantes 329) (paris va-paris 396)))
(setf (get 'nantes 'adj) '((limoges va-limoges 329) (bordeaux va-bordeaux 329) (rennes va-rennes 107)))
(setf (get 'paris 'adj) '((calais va-calais 297) (nancy va-nancy 372) (dijon va-dijon 313) (limoges va-limoges 396) (rennes va-rennes 348) (caen va-caen 241)))

; (setf (get 'island 'latlong) '(40 40 0 N 7 7 7 E))
(setf (get 'nice 'latlong) '(43 42 0 N 7 21 0 E))
(setf (get 'rennes 'latlong) '(48 07 0 N 1 02 0 W))
(setf (get 'brest 'latlong) '(48 24 0  N 4 30 0 W))
(setf (get 'strasbourg 'latlong) '(48 32 24 N 7 37 34 E))
(setf (get 'dijon 'latlong) '(47 21 0 N 5 02 0 E))
(setf (get 'lyon 'latlong) '(45 44 0 N 4 52 0 E))
(setf (get 'grenoble 'latlong) '(45 21 36 N 5 19 12 E))
(setf (get 'avignon 'latlong) '(43 50 0 N 4 45 0 E))
(setf (get 'marseille 'latlong) '(43 18 0 N 5 25 0 E))
(setf (get 'bordeaux 'latlong) '(44 50 0 N 0 37 0 W))
(setf (get 'toulouse 'latlong) '(43 37 0 N 1 27 0 E))
(setf (get 'montpellier 'latlong) '(43 38 0 N 3 53 0 E))
(setf (get 'limoges 'latlong) '(45 30 0 N 1 10 0 E))
(setf (get 'caen 'latlong) '(49 15 0 N 0 20 0 W))
(setf (get 'calais 'latlong) '(50 57 36 N 1 57 0 E))
(setf (get 'nancy 'latlong) '(48 50 0 N 6 10 0 E))
(setf (get 'nantes 'latlong) '(47 15 0 N 1 30 0 W))
(setf (get 'paris 'latlong) '(48 51 0 N 2 20 0 E))


; 1 degree = 60' = 111 km --> 1' = 1.85km --> 1'' = 0.031 km
(defun est-distance (city1 city2) 
	(let* ((deg 111) 
		  (m 1.85)
		  (s .031)
		  (lat-km1 (+ (* (first city1) deg) (* (second city1) m) (* (third city1) s)))
		  (long-km1 (+ (* (fifth city1) deg) (* (sixth city1) m) (* (seventh city1) s)))
		  (lat-km2 (+ (* (first city2) deg) (* (second city2) m) (* (third city2) s)))
		  (long-km2 (+ (* (fifth city2) deg) (* (sixth city2) m) (* (seventh city2) s)))
		  (a (abs (- lat-km1 lat-km2)))
		  (b 0))
		(if (eq (eighth city1) (eighth city2))
			 (setf b (abs (- long-km1 long-km2)))
			  (setf b (+ long-km1 long-km2)))
		(sqrt (+ (* a a) (* b b)))))


; -----------------------------PROBLEM-SPECIFIC FUNCTIONS-------------------------------------

#|
caveat: the successor function does not need information from the "problem" data structure, 
but to make the interface for successors generic to all problems, we need to pass it in, 
following Russell & Norvig.
|#
(defun successors (problem state) 
	(get state 'adj))

; creates child-node with updated path and path cost for each successor
(defun make-child (current successor)
	(if (eq nil (second successor)) 
		(list (car successor) (list (second successor)) (+ (third successor) (third current)))
		(list (car successor) (append (second current) (list (second successor))) (+ (third successor) (third current)))))

(defun goal-test (problem state)
	(cond ((eq (get 'problem 'goal-state) state) t)
		   (t nil)))

; used by ucs
; g(n) = path cost, no separate function needed bc already calculated in make-child

; used by gbfs
; estimated straight-line distance of cheapest path from successor to goal state
(defun h (node goal)
	(est-distance (get (car node) 'latlong) (get goal 'latlong))) 

; used by a*
; f(n) = pathcost + estimated distance from successor to goal state
(defun f (node goal)
	(+ (third node) (h node goal))) 


(defun insert-and-eliminate-ucs (child frontier)
	(cond ((null frontier) (list child))
		  ((and (eq (car child) (first (car frontier))) (> (third child) (third (car frontier)))) frontier)
		  ((and (eq (car child) (first (car frontier))) (< (third child) (third (car frontier)))) (substitute child (car frontier) frontier))
		  ((and (< (third child) (third (car frontier))) (member (car child) frontier :key #'first)) (cons child (remove (car child) frontier :key #'first)))
		  ((< (third child) (third (car frontier))) (cons child frontier))
		  (t (cons (car frontier) (insert-and-eliminate-ucs child (cdr frontier))))))


(defun insert-and-eliminate-gbfs (child frontier)
	(cond ((null frontier) (list child))
		  ((< (first child) (first (car frontier))) (cons child frontier))
		  (t (cons (car frontier) (insert-and-eliminate-gbfs child (cdr frontier))))))




