;;; <Li-Yun> <Wang>
;;; <date submitted: 10-01-2016>
;;; CS 541
;;; Lab 1

; small function
(defun The_Tower(ndisk beg aux end)
    (when (= ndisk 1)
	; Step 2: (The_Tower 1 beg aux end)
	(format t "~%Move One Disc from peg ~D to peg ~D." beg end)
    )
    (when (/= ndisk 1)
	;(print 11)
	(setq ndisk (- ndisk 1))
	; Step 1: (The_Tower (ndisk - 1) beg end aux)
	(The_Tower ndisk beg end aux)
	; Step 2: (The_Tower 1 beg aux end)
	(The_Tower 1 beg aux end)
	; Step 3: (The_Tower (ndisk - 1) aux beg end)
	(The_Tower ndisk aux beg end)
    )
)

; define main function
(defun main_function()
    ; Declare Variables
    (print "Please determine disk numbers: ")
    (setq ndisk (read))
    (setq beg "A")
    (setq aux "B")
    (setq end "C")
    
    ; display parameters
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
    (terpri)

    ; run execution function
    (print "Starts The Algorithm:")
    (The_Tower ndisk beg aux end)
    (print "Done!!")
)

; run main function
(main_function)




