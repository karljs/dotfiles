;;; wrap.el --- Wrap stuff with delimiters
;; Version: 0.1.0

;;;###autoload
(defun wrap-region (delim)
  "Wrap a region with some kind of enclosing delimiters.  Doesn't
use interactive 'r' because it's tacky."
  (interactive "sWrap with: ")
  (message "%s" delim)
  (if (use-region-p)
    (cond
     ((or (string= delim "(") (string= delim ")"))
      (wrap-region-with "(" ")"))
     (t
      (wrap-region-with delim delim)))
    (message "No region selected")))

(defun wrap-region-with (lft rgt)
  "Wrap region given a delimiter"
  (save-excursion
    (goto-char (region-end))
    (insert rgt)
    (goto-char (region-beginning))
    (insert lft)
    (deactivate-mark)))

;;; wrap.el ends here
