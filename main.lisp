

(setf (get 'problem 'start-state) 'brest)
(setf (get 'problem 'goal-state) 'marseille)

; uninformed, optimal, complete
(defun ucs (problem)
	(unless (or (null (get (get 'problem 'start-state) 'adj)) (null (get (get 'problem 'goal-state) 'adj)))
		 (do* ((explored '())
			  (frontier (list (list (get 'problem 'start-state) '() 0))) 
			  (current (pop frontier) (pop frontier)))

			  ((goal-test problem (car current)) (cdr current))
			   
			   (if (not (member (car current) explored))
			   		(setf explored (append explored (list (car current)))))
			   (dolist (successor (successors problem (car current)))
			   		(let ((child (make-child current successor)))
				   		(if (null frontier) 
				  			(setf frontier (remove nil (list child frontier)))
					  		(if (not (member (car child) explored))
								(setf frontier (insert-and-eliminate-ucs child frontier)))))))))

; informed (uses heuristic h(n)), not optimal, incomplete
(defun gbfs (problem)
	(unless (or (null (get (get 'problem 'start-state) 'adj)) (null (get (get 'problem 'goal-state) 'adj)))
		(do* ((explored '())
			  ((frontier (list (list (h (list (get 'problem 'start-state) '() 0) (get 'problem 'goal-state)) (list (get 'problem 'start-state) '() 0)))))
			  (current (pop frontier) (pop frontier)))

			  ((goal-test problem (car (second current))) (cdr (second current)))

			   (if (not (member (cadr current) explored))
			   		(setf explored (append explored (list (car (second current))))))
			   (dolist (successor (successors problem (car (second current))))
		   			(if (and (not (member (car successor) explored)) (not (member (car successor) frontier :key #'first)))
			   			(progn
			   				(let ((child (make-child (second current) successor)))
					   			(setf child (cons (h child (get 'problem 'goal-state)) (list child)))
					   			(if (null frontier)
					  				(setf frontier (list child))
									(setf frontier (insert-and-eliminate-gbfs child frontier))))))))))

; informed (uses heuristic f(n)), optimal, complete
(defun a* (problem)
	(unless (or (null (get (get 'problem 'start-state) 'adj)) (null (get (get 'problem 'goal-state) 'adj)))
		(do* ((explored '())
			  ((frontier (list (list (h (list (get 'problem 'start-state) '() 0) (get 'problem 'goal-state)) (list (get 'problem 'start-state) '() 0)))))
			  (current (pop frontier) (pop frontier)))

			  ((goal-test problem (car (second current))) (cdr (second current)))
			   
			   (if (not (member (cadr current) explored))
			   		(setf explored (append explored (list (car (second current))))))
			   (dolist (successor (successors problem (car (second current))))
		   			(if (and (not (member (car successor) explored)) (not (member (car successor) frontier :key #'first)))
			   			(progn
			   				(let ((child (make-child (second current) successor)))
					   			(setf child (cons (f child (get 'problem 'goal-state)) (list child)))
					   			(if (null frontier)
					  				(setf frontier (list child))
									(setf frontier (insert-and-eliminate-gbfs child frontier))))))))))



#|#
instument a copy of each search algorithm and compute over the search: 

the average number of nodes explored or entered
the average number of nodes expanded (i.e., the total number of successors)
the average number of nodes maintained (i.e., stored in the frontier)
the average number of times it found the optimal solution
|#

(defun ucs-stats (problem)
	(unless (or (null (get (get 'problem 'start-state) 'adj)) (null (get (get 'problem 'goal-state) 'adj)))
		(let ((ENTERED 0)
			  (EXPANDED 0)
			  (MAINTAINED 0)
			  (SOLUTION nil))
			(setf SOLUTION (do* ((explored '())
							  (frontier (list (list (get 'problem 'start-state) '() 0))) 
							  (current (pop frontier) (pop frontier)))

							  ((goal-test problem (car current)) (cdr current))
							   
							   (if (not (member (car current) explored))
							   		(progn 
							   			(setf explored (append explored (list (car current))))
							   			(setf ENTERED (+ entered 1))))
							   (dolist (successor (successors problem (car current)))
							   		(setf EXPANDED (+ EXPANDED 1))
							   		(let ((child (make-child current successor)))
								   		(if (null frontier) 
								   			(progn 
								  				(setf frontier (list child))
								  				(setf MAINTAINED (+ MAINTAINED 1)))
									  		(if (not (member (car child) explored))
									  			(progn 
									  				(setf old-frontier frontier)
									  				(setf frontier (insert-and-eliminate-ucs child frontier))
													(if (not (eq old-frontier frontier))
														(setf MAINTAINED (+ MAINTAINED 1))))))))))
			(list ENTERED EXPANDED MAINTAINED SOLUTION))))


(defun gbfs-stats (problem)
	(unless (or (null (get (get 'problem 'start-state) 'adj)) (null (get (get 'problem 'goal-state) 'adj)))
		(let ((ENTERED 0)
		  (EXPANDED 0)
		  (MAINTAINED 0)
		  (SOLUTION nil))
		(setf SOLUTION (do* ((explored '())
						  ((frontier (list (list (h (list (get 'problem 'start-state) '() 0) (get 'problem 'goal-state)) (list (get 'problem 'start-state) '() 0)))))
						  (current (pop frontier) (pop frontier)))

						  ((goal-test problem (car (second current))) (cdr (second current)))

						   (if (not (member (cadr current) explored))
						   		(progn
						   			(setf explored (append explored (list (car (second current)))))
						   			(setf ENTERED (+ entered 1))))
						   (dolist (successor (successors problem (car (second current))))
						   		(setf EXPANDED (+ EXPANDED 1))
					   			(if (and (not (member (car successor) explored)) (not (member (car successor) frontier :key #'first)))
						   			(progn
						   				(let ((child (make-child (second current) successor)))
								   			(setf child (cons (h child (get 'problem 'goal-state)) (list child)))
								   			(if (null frontier)
								   				(progn 
								  					(setf frontier (list child))
								  					(setf MAINTAINED (+ MAINTAINED 1)))
								   				(progn 
													(setf frontier (insert-and-eliminate-gbfs child frontier))
													(setf MAINTAINED (+ MAINTAINED 1))))))))))
		(list ENTERED EXPANDED MAINTAINED SOLUTION))))


(defun a*-stats (problem)
	(unless (or (null (get (get 'problem 'start-state) 'adj)) (null (get (get 'problem 'goal-state) 'adj)))
		(let ((ENTERED 0)
			  (EXPANDED 0)
			  (MAINTAINED 0)
			  (SOLUTION nil))
			(setf SOLUTION (do* ((explored '())
							  ((frontier (list (list (h (list (get 'problem 'start-state) '() 0) (get 'problem 'goal-state)) (list (get 'problem 'start-state) '() 0)))))
							  (current (pop frontier) (pop frontier)))

							  ((goal-test problem (car (second current))) (cdr (second current)))
							   
							   (if (not (member (cadr current) explored))
							   		(progn 
							   			(setf explored (append explored (list (car (second current)))))
							   			(setf ENTERED (+ entered 1))))
							   (dolist (successor (successors problem (car (second current))))
							   		(setf EXPANDED (+ EXPANDED 1))
						   			(if (and (not (member (car successor) explored)) (not (member (car successor) frontier :key #'first)))
							   			(progn
							   				(let ((child (make-child (second current) successor)))
									   			(setf child (cons (f child (get 'problem 'goal-state)) (list child)))
									   			(if (null frontier)
									  				(progn 
									  					(setf frontier (list child))
									  					(setf MAINTAINED (+ MAINTAINED 1)))
													(progn 
														(setf frontier (insert-and-eliminate-gbfs child frontier))
														(setf MAINTAINED (+ MAINTAINED 1))))))))))
			(list ENTERED EXPANDED MAINTAINED SOLUTION))))



#|
Using each of these implementations, find the path between the following initial and goal cities:

Run	Initial State	Goal State
1	Brest			Marseille
2	Montpellier		Calais
3	Strasbourg		Bordeaux
4	Paris			Grenoble
5	Grenoble		Paris
6	Brest			Grenoble
7	Grenoble		Brest
|#

(setf algorithms '(ucs-stats gbfs-stats a*-stats))
(setf cities '((Brest Marseille) (Montpellier Calais) (Strasbourg Bordeaux) (Paris Grenoble) 
			   (Grenoble Paris) (Brest Grenoble) (Grenoble Brest)))


(defun all-stats (algorithms cities)
	(let ((all-outputs '())
		  (avg-entered 0)
		  (avg-expanded 0)
		  (avg-maintained 0)
		  (all-avg-stats '()))
		(setf all-outputs (dolist (algo algorithms all-outputs)
							(let ((outputs '()))				
							 (setf all-outputs (append all-outputs (dolist (pair cities (list outputs))
																		(setf (get 'problem 'start-state) (first pair))
																		(setf (get 'problem 'goal-state) (second pair))
																		(setf outputs (append outputs (list (funcall algo 'problem))))))))))
	(dolist (section all-outputs (format t "averaged stats of all algorithms: ~a~%" all-avg-stats))
			(format t "section: ~a~%~%" section)
			(format t "nodes entered during each problem: ~a~%" (mapcar #'first section))
			(format t "nodes expanded during each problem: ~a~%" (mapcar #'second section))
			(format t "nodes maintained during each problem: ~a~%" (mapcar #'third section))
			(setf all-avg-stats (append all-avg-stats (do* ((avg-stats '())
															(bool-solution-list '())) 

															((not (null avg-stats)) (list avg-stats))

															 (setf avg-stats (append avg-stats (list (float (/ (apply #'+ (mapcar #'first section)) (list-length section))))))
															 (setf avg-stats (append avg-stats (list (float (/ (apply #'+ (mapcar #'second section)) (list-length section))))))
															 (setf avg-stats (append avg-stats (list (float (/ (apply #'+ (mapcar #'third section)) (list-length section))))))
															 (setf bool-solution-list (dolist (solution (mapcar #'fourth section) bool-solution-list)
															 								(if solution
															 									(setf bool-solution-list (append bool-solution-list (list 1)))
															 									(setf bool-solution-list (append bool-solution-list (list 0))))))
															 (setf avg-stats (append avg-stats (list (float (/ (apply #'+ bool-solution-list) (list-length section))))))
															 (format t "averaged stats of the algorithm: ~a~%~%" avg-stats)))))))
















