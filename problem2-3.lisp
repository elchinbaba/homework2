(defun set-clauses (expression)
  (cond ((null expression)
    nil)
    (t
      (setq clause_ (car expression))
      (setq literals_ (cdr clause_))
      (cons literals_ (set-clauses (cdr expression))))))

(defun is-in (prev-literals_ literal_)
  (loop (setq cur-literal_ (car prev-literals_))
    (when (null cur-literal_) (return nil))

    (cond ((equal cur-literal_ literal_) (return t)))

    (setq prev-literals_ (cdr prev-literals_))))

(defun set-literals (clauses_ &optional prev-literals)
  (cond ((null clauses_)
    prev-literals)
    (t
      (setq literals_ (car clauses_))
      (cond ((null (cdr literals_))
        (setf literal_ (car literals_))

        (cond ((and (not (null prev-literals)) (is-in prev-literals literal_))
          (set-literals (cdr clauses_) prev-literals))
            ((and (not (null prev-literals)) (is-in prev-literals (negated-literal literal_)))
          1)
          (t
            (set-literals (cdr clauses_) (cons literal_ prev-literals))))) ; (cons literal_ (set-literals (cdr clauses_) prev-literals))
        (t
          (set-literals (cdr clauses_) prev-literals))))))

(defun set-non-literals (clauses_)
  (cond ((null clauses_)
    nil)
    (t
      (setf literals_ (car clauses_))
      (cond ((null (cdr literals_))
        (set-non-literals (cdr clauses_))
      )
        (t
          (cons literals_ (set-non-literals (cdr clauses_))))))))

(defun remove-literal (clause_ literal_)
  (cond ((null clause_)
    nil)
    (t
      (cond ((not (equal literal_ (car clause_)))
        (cons (car clause_) (remove-literal (cdr clause_) literal_)))
        (t
          (remove-literal (cdr clause_) literal_))))))

(defun negated-literal (literal_)
  (cond ((listp literal_)
    (car (cdr literal_)))
    (t
      (cons 'not (cons literal_ nil)))))

(defun update-non-literals (clauses_ literals_)
  (cond ((null clauses_)
    nil)
    (t
      (setf perm-literals literals_)
      (setf clause_ (car clauses_))
      ;;(print clauses_)

      (loop (setq literal_ (car literals_))
        (when (null literal_) (return))

        (setf clause_ (remove-literal clause_ (negated-literal literal_)))

        (setf literals_ (cdr literals_)))

      (cond ((null clause_)
        (update-non-literals (cdr clauses_) perm-literals))
        (t
          (cons clause_ (update-non-literals (cdr clauses_) perm-literals)))))))

(defun check (non-literals_ literals_)
  (loop (setf cur-non-literal_ (car non-literals_))
    (when (null cur-non-literal_) (return 0))
    (cond ((null (cdr cur-non-literal_))
      (setf literal_ (car cur-non-literal_))
      (setf temp-literals_ (cons (car literals_) (cdr literals_)))
      (loop (setf cur-literal_ (car temp-literals_))
        (when (null cur-literal_) nil)
        (setf n-literal_ (negated-literal cur-literal_))
        (cond ((equal n-literal_ cur-literal_)
          (return 1)))
        (setf temp-literals_ (cdr temp-literals_)))))
    (setf non-literals_ (cdr non-literals_))))

(defun resolution-refutation (CNF-log-exp)
  (setf exp (cdr CNF-log-exp))
  (setf temp-exp (cdr CNF-log-exp))
  (cond ((null exp)
    nil)
    (t
      (setf clauses (set-clauses exp))
      (setf non-literals (set-non-literals clauses))
      (setf literals (set-literals clauses))

      ;; (print non-literals)
      ;; (print literals)
      (do ((x 1 (+ x 1)))
        ((or (null non-literals) (> x 5)) nil)
        (setf non-literals (update-non-literals non-literals literals))
        ;;(print non-literals)

        (setf literals (set-literals non-literals literals))
        (print literals)
        (cond ((= literals 1)
          0))
        ;;(setf is-end (check non-literals literals))
        ;;(cond ((= is-end 1) (return 1)))
        ;;(print is-end)
        (setf non-literals (set-non-literals non-literals))
        ;; (print non-literals)

        )
      )))

;; (resolution-refutation '(and (or C) (or A) (or A B (not C)) (or (not A) D) (or (not B))))

(resolution-refutation '(and (or P) (or Q) (or (not P) R) (or (not Q) (not R) S) (or (not S))))
