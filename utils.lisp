(in-package :regvaldefs)

(defun prepend/reduce-equiv (what fn to &key identity (test #'eql))
  "Reduce WHAT with FN, then APPEND whatever is produced with TO, unless
     that which is produced is an identity transform of identity,
     with regard to test."
  (let ((appendee (reduce fn what :initial-value identity)))
    (if (funcall test appendee identity)
        to
        (append (list appendee) to))))

(defun eval-if (predicate form)
  "XXX: rename to MAYBE-EVAL?"
  (if predicate
      (eval form)
      form))
