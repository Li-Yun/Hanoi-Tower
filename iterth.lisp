; <Li-Yun> <Wang>
; <date submitted: 10-02-2016>
; CS 541
; Lab 1

; execute the algorithm with even disks
(defun The_Tower_Algorithm_Even (total_moving ndisk beg aux end)
    ; declare variables
    (setq beg_stack (list 0))
    (setq aux_stack (list 0))
    (setq end_stack (list 0))
    (setq counter ndisk)
    (loop for stack_index from 1 to ndisk do
        (setq beg_stack (cons counter beg_stack))
        (setq counter (- counter 1))
    )
    
    ; for loop
    (loop for i from 1 to total_moving do
	; Step 1: make the legal move between pegs beg and aux
	(when (= (mod i 3) 1)
            (cond ((and (> (car aux_stack) (car beg_stack)) (/= (car beg_stack) 0))  ; move one disk from beg peg to aux peg
                (format t "~%Move One Disc from peg ~D to peg ~D." beg aux)
                (setq tmp_value (car beg_stack))  ; get the top element
                (setq beg_stack (cdr beg_stack))  ; pop the top from beg stack
                (setq aux_stack (cons tmp_value aux_stack))  ; push element to end stack
                )
                ((and (> (car beg_stack) (car aux_stack)) (/= (car aux_stack) 0))    ; move one disk from aux peg to beg peg
                (format t "~%Move One Disc from peg ~D to peg ~D." aux beg)
                (setq tmp_value (car aux_stack))  ; get the top element
                (setq aux_stack (cdr aux_stack))  ; pop the top from end stack
                (setq beg_stack (cons tmp_value beg_stack))  ; push element to beg stack
                )
		( (= (car beg_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." aux beg)
                (setq tmp_value (car aux_stack))  ; get the top element
                (setq aux_stack (cdr aux_stack))  ; pop the top from end stack
                (setq beg_stack (cons tmp_value beg_stack))  ; push element to beg stack
		)
		( (= (car aux_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." beg aux)
		(setq tmp_value (car beg_stack))  ; get the top element
                (setq beg_stack (cdr beg_stack))  ; pop the top from beg stack
                (setq aux_stack (cons tmp_value aux_stack))  ; push element to end stack
		)
            )
        )
	; Step 2: make the legal move between pegs beg and end
	(when (= (mod i 3) 2)
            (cond ((and (> (car end_stack) (car beg_stack)) (/= (car beg_stack) 0))  ; move one disk from beg peg to end peg
                (format t "~%Move One Disc from peg ~D to peg ~D." beg end)
                (setq tmp_value (car beg_stack))  ; get the top element
                (setq beg_stack (cdr beg_stack))  ; pop the top from beg stack
                (setq end_stack (cons tmp_value end_stack))  ; push element to end stack
                )
                ((and (> (car beg_stack) (car end_stack)) (/= (car end_stack) 0))   ; move one disk from end peg to beg peg
                (format t "~%Move One Disc from peg ~D to peg ~D." end beg)
                (setq tmp_value (car end_stack))  ; get the top element
                (setq end_stack (cdr end_stack))  ; pop the top from aux stack
                (setq beg_stack (cons tmp_value beg_stack))  ; push element to beg stack
                )
		( (= (car beg_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." end beg)
                (setq tmp_value (car end_stack))  ; get the top element
                (setq end_stack (cdr end_stack))  ; pop the top from aux stack
                (setq beg_stack (cons tmp_value beg_stack))  ; push element to beg stack
		)
		( (= (car end_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." beg end)
                (setq tmp_value (car beg_stack))  ; get the top element
                (setq beg_stack (cdr beg_stack))  ; pop the top from beg stack
                (setq end_stack (cons tmp_value end_stack))  ; push element to end stack
		)
            )
        )
	; Step 3: make the legal move between pegs aux and end
	(when (= (mod i 3) 0)
            (cond ((and (> (car end_stack) (car aux_stack)) (/= (car aux_stack) 0))  ; move one disk from aux peg to end peg
                (format t "~%Move One Disc from peg ~D to peg ~D." aux end)
                (setq tmp_value (car aux_stack))  ; get the top element
                (setq aux_stack (cdr aux_stack))  ; pop the top from aux stack
                (setq end_stack (cons tmp_value end_stack))  ; push element to end stack
                )
                ((and (> (car aux_stack) (car end_stack)) (/= (car end_stack) 0))    ; move one disk from end peg to aux peg
                (format t "~%Move One Disc from peg ~D to peg ~D." end aux)
                (setq tmp_value (car end_stack))  ; get the top element
                (setq end_stack (cdr end_stack))  ; pop the top from end stack
                (setq aux_stack (cons tmp_value aux_stack))  ; push element to aux stack
                )
		( (= (car aux_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." end aux)
                (setq tmp_value (car end_stack))  ; get the top element
                (setq end_stack (cdr end_stack))  ; pop the top from end stack
                (setq aux_stack (cons tmp_value aux_stack))  ; push element to aux stack
		)
		( (= (end_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." aux end)
                (setq tmp_value (car aux_stack))  ; get the top element
                (setq aux_stack (cdr aux_stack))  ; pop the top from aux stack
                (setq end_stack (cons tmp_value end_stack))  ; push element to end stack
		)
            )
        )
    )
)

; execute the algorithm with odd disks
(defun The_Tower_Algorithm_Odd (total_moving ndisk beg aux end)
    ; declare variables
    (setq beg_stack (list 0))
    (setq aux_stack (list 0))
    (setq end_stack (list 0))
    (setq counter ndisk)
    (loop for stack_index from 1 to ndisk do
	(setq beg_stack (cons counter beg_stack))
	(setq counter (- counter 1))
    )

    ; for loop
    (loop for i from 1 to total_moving do
	; Step 1: make the legal move between pegs beg and end
        (when (= (mod i 3) 1)
	    (cond ( (< (car end_stack) (car beg_stack))  ; move one disk from beg peg to end peg
		(format t "~%Move One Disc from peg ~D to peg ~D." beg end)
		(setq tmp_value (car beg_stack))  ; get the top element
		(setq beg_stack (cdr beg_stack))  ; pop the top from beg stack
		(setq end_stack (cons tmp_value end_stack))  ; push element to end stack
	        )
                ( (< (car beg_stack) (car end_stack))  ; move one disk from end peg to beg peg
		(format t "~%Move One Disc from peg ~D to peg ~D." end beg)
                (setq tmp_value (car end_stack))  ; get the top element
                (setq end_stack (cdr end_stack))  ; pop the top from end stack
                (setq beg_stack (cons tmp_value beg_stack))  ; push element to beg stack
                )
		( (= (car beg_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." end beg)
		(setq tmp_value (car end_stack))  ; get the top element
		(setq end_stack (cdr end_stack))  ; pop the top from end stack
                (setq beg_stack (cons tmp_value beg_stack))  ; push element to beg stack)  
		)
		( (= (car end_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." beg end)
		(setq tmp_value (car beg_stack))  ; get the top element
                (setq beg_stack (cdr beg_stack))  ; pop the top from beg stack
                (setq end_stack (cons tmp_value end_stack))  ; push element to end stack
		)
	    )
	)
	; Step 2: make the legal move between pegs beg and aux
	(when (= (mod i 3) 2)
	    (cond ( (< (car aux_stack) (car beg_stack))  ; move one disk from beg peg to aux peg
	        (format t "~%Move One Disc from peg ~D to peg ~D." beg aux)
	        (setq tmp_value (car beg_stack))  ; get the top element
	        (setq beg_stack (cdr beg_stack))  ; pop the top from beg stack
	        (setq aux_stack (cons tmp_value aux_stack))  ; push element to aux stack
	        )
	        ( (< (car beg_stack) (car aux_stack))  ; move one disk from aux peg to beg peg
	        (format t "~%Move One Disc from peg ~D to peg ~D." aux beg)
	        (setq tmp_value (car aux_stack))  ; get the top element
	        (setq aux_stack (cdr aux_stack))  ; pop the top from aux stack
	        (setq beg_stack (cons tmp_value beg_stack))  ; push element to beg stack
	        )
		( (= (car beg_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." aux beg)
                (setq tmp_value (car aux_stack))  ; get the top element
                (setq aux_stack (cdr aux_stack))  ; pop the top from aux stack
                (setq beg_stack (cons tmp_value beg_stack))  ; push element to beg stack
		)
		( (= (car aux_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." beg aux)
                (setq tmp_value (car beg_stack))  ; get the top element
                (setq beg_stack (cdr beg_stack))  ; pop the top from beg stack
                (setq aux_stack (cons tmp_value aux_stack))  ; push element to aux stack
		)
	    )
	)
	; Step 3: make the legal move between pegs end and aux
	(when (= (mod i 3) 0)
	    (cond ( (> (car end_stack) (car aux_stack))  ; move one disk from aux peg to end peg
	        (format t "~%Move One Disc from peg ~D to peg ~D." aux end)
	        (setq tmp_value (car aux_stack))  ; get the top element
	        (setq aux_stack (cdr aux_stack))  ; pop the top from aux stack
	        (setq end_stack (cons tmp_value end_stack))  ; push element to end stack
	        )
	        ( (> (car aux_stack) (car end_stack))  ; move one disk from end peg to aux peg
	        (format t "~%Move One Disc from peg ~D to peg ~D." end aux)
	        (setq tmp_value (car end_stack))  ; get the top element
	        (setq end_stack (cdr end_stack))  ; pop the top from end stack
	        (setq aux_stack (cons tmp_value aux_stack))  ; push element to aux stack
	        )
		( (= (car aux_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." end aux)
		(setq tmp_value (car end_stack))  ; get the top element
                (setq end_stack (cdr end_stack))  ; pop the top from end stack
                (setq aux_stack (cons tmp_value aux_stack))  ; push element to aux stack
		)
		( (= (end_stack) 0)
		(format t "~%Move One Disc from peg ~D to peg ~D." aux end)
                (setq tmp_value (car aux_stack))  ; get the top element
                (setq aux_stack (cdr aux_stack))  ; pop the top from aux stack
                (setq end_stack (cons tmp_value end_stack))  ; push element to end stack
		)
	    )
	)
    )
)

; define the algorithm of the tower (iteration version)
(defun The_Tower_Iter(ndisk beg aux end)
    ; calculate total move number
    (setq total_moving (- (expt 2 ndisk) 1))
    (print "==========================")

    (print "Starts With The Algorithm")
    ; condition 1, if the # of disks is even
    (if (= (mod ndisk 2) 0)
	(The_Tower_Algorithm_Even total_moving ndisk beg aux end)
    )
    ; condition 2, if the # of disks is odd
    (if (/= (mod ndisk 2) 0)
	(The_Tower_Algorithm_Odd total_moving ndisk beg aux end)
    )
    (print "Done!!") 
)

; define the main function
(defun main_function()
    ; declare variables
    (print "Please determine disk numbers: ")
    (setq ndisk (read))
    (setq beg "A")
    (setq aux "B")
    (setq end "C")

    ; display variable values
    (princ "Disk Number: ")
    (write ndisk)
    (terpri)
    (princ "Beg Peg: ")
    (write beg)
    (terpri)
    (princ "Aux Peg: ") 
    (write aux)
    (terpri)
    (princ "End Peg: ") 
    (write end)

    ; run the algorithm
    (The_Tower_Iter ndisk beg aux end)
)

; run the main function
(main_function)
