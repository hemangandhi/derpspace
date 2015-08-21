;;;;lisp utilities for iser io

(defun print-sentance (subject verb victim passive?)
    (if passive?
        (prompt (concatenate 'string verb " got done to " victim " by " subject))
	(prompt (concatenate 'string subject " does " verb " to the " victim))))

(defun read-list (&rest args)
    (values (read-from-string
		(concatenate 'string "(" (apply #'read-line args) ")"))))

(defun prompt (&rest args)
    (apply #'format *query-io* args)
    (read-list *query-io*))