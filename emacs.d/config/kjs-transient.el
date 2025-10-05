;;; -*- lexical-binding: t; -*-

;;; kjs-deb.el --- code to simplify Debian/Ubuntu package maintenance

;;; Commentary

;; This is a relatively simple custom infix for the `transient'
;; package that implements a three-state command line option,
;; essentially a hybrid between `transient' classes `switch' and
;; `option'.
;;
;; The three states are:
;; 1. Off
;; 2. On, with no value set
;; 3. On, with a string value set
;;
;; This was originally implemented for the `-I` argument in
;; `dpkg-source', which has some defaults that are used when no
;; pattern is provided, but also allows custom patterns.

;;; Code

(defclass kjs--transient-tristate (transient-infix)
  ()
  "A 3-state transient infix: off, on but empty, or on with value.")


(cl-defmethod transient-infix-read ((obj kjs--transient-tristate))
  "Cycle through states or prompt for value."
  (let ((current (oref obj value)))
    (pcase current
      ('nil t)
      ((pred (eq t))
       (read-string (concat (oref obj description) ": ")))
      (_ nil))))

(cl-defmethod transient-init-value ((obj kjs--transient-tristate))
  "Initialize the value from the prefix's :value."
  (let ((argument (oref obj argument))
        (value (oref transient--prefix value)))
    (oset obj value
          (cond
           ((member argument value) t)
           ((seq-find
             (lambda (v)
               (and (stringp v)
                    (string-prefix-p argument v))) value)
            (substring (seq-find
                        (lambda (v)
                          (and (stringp v)
                               (string-prefix-p argument v))) value)
                       (length argument)))
           (t nil)))))

(cl-defmethod transient-format-value ((obj kjs--transient-tristate))
  "Format the current state for display.
This is how it's actually rendered in the UI, rather than what data is
passed along to the suffix."
  (let ((value (oref obj value)))
    (propertize
     (pcase value
       ('nil "[ ]")
       ((pred (eq t)) "[✓]")
       (_ (format "[✓:%s]" value)))
     'face (if value 'transient-value 'transient-inactive-value))))


(cl-defmethod transient-infix-value ((obj kjs--transient-tristate))
  "Format the argument value for the args list.
By default, this would just return `t' or just the string data, but
that's not enough to actually use or even distinguish from other values."
  (let ((value (oref obj value))
        (argument (oref obj argument)))
    (cond
     ((eq value nil) nil)
     ((eq value t) argument)
     (t (format "%s%s" argument value)))))


(provide 'kjs-transient)


;;; kjs-transient.el ends here
