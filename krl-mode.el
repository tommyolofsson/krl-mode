(defvar krl-mode-hook nil)
(defvar krl-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for KRL major mode")

(add-to-list 'auto-mode-alist '("\\.src\\'" . krl-mode))
(add-to-list 'auto-mode-alist '("\\.dat\\'" . krl-mode))
(add-to-list 'auto-mode-alist '("\\.sub\\'" . krl-mode))

(defun rstrip (str)
  "Strip tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos) "" str))

;; Blocks allowed only at top level.
;; TODO: These should be regexes as well. (If trailing ws should be allowed...)
(defconst krl-keywords-toplevel-block-pairs
  '(
    ("DEF " "END\n")
    ("DEFFCT " "ENDFCT\n")
    ))

(defun regexp-opt-allow-ind (str-list &optional prefix)
  "Optimize regex for a list of strings, allowing for leading whitespace."
  (concat "^ *" prefix (regexp-opt str-list t)))

(defun list-nth (i pair-list)
  (mapcar (lambda (x) (nth i x)) pair-list))

(defconst krl-indent-regex-toplevel-block-open
  (regexp-opt-allow-ind (list-nth 0 krl-keywords-toplevel-block-pairs) "\\(GLOBAL +\\)?"))
(defconst krl-indent-regex-toplevel-block-close
  (regexp-opt-allow-ind (list-nth 1 krl-keywords-toplevel-block-pairs)))

;; Blocks not allowed at top level. Arb. nestable.
;; The whitespace indicates expected
(defconst krl-keywords-nested-block-pairs
  '(
    ;; Loops
    ("FOR " "ENDFOR\n")
    ("WHILE " "ENDWHILE\n")
    ("REPEAT\n" "UNTIL ")
    ("LOOP\n" "ENDLOOP\n")
    ;; Branches
    ("IF " "ENDIF\n")
    ("SWITCH " "ENDSWITCH\n")
    ("SPLINE" "ENDSPLINE\n")
    ))

(defconst krl-keywords-nested-block-middle
  '(
    ("IF " "ENDIF\n" "ELSE\n")
    ("SWITCH " "ENDSWITCH\n" "CASE ")
    ))

(defconst krl-indent-regex-nested-block-middle
  (regexp-opt-allow-ind (list-nth 2 krl-keywords-nested-block-middle)))

(defconst krl-indent-regex-nested-block-open
  (regexp-opt-allow-ind (append (list-nth 0 krl-keywords-nested-block-pairs))))

(defconst krl-indent-regex-nested-block-close
  (regexp-opt-allow-ind (list-nth 1 krl-keywords-nested-block-pairs)))

;; See SI p360.
(defconst krl-keywords-misc
  '(
    "DEFDAT"
    "ENDDAT"
    "DECL"
    "GLOBAL"
    "PUBLIC"
    "THEN"
    "ENUM"
    "NOT"
    "AND"
    "OR"
    "EXOR"
    "B_NOT"
    "B_AND"
    "B_OR"
    "B_EXOR"
    "INTERRUPT"
    "WHEN"
    "DO"
    "STEP"
    "WHEN"
    "RETURN"
    "STRUC"
    "TO"
    ))

(defconst krl-keywords
  (append
  (mapcar (lambda (x) (rstrip (nth 0 x))) krl-keywords-toplevel-block-pairs)
  (mapcar (lambda (x) (rstrip (nth 1 x))) krl-keywords-toplevel-block-pairs)
  (mapcar (lambda (x) (rstrip (nth 0 x))) krl-keywords-nested-block-pairs)
  (mapcar (lambda (x) (rstrip (nth 1 x))) krl-keywords-nested-block-pairs)
  (mapcar (lambda (x) (rstrip (nth 2 x))) krl-keywords-nested-block-middle)
  krl-keywords-misc
   ))

(defconst krl-warning-keywords
  '(
    "HALT"
    "BRAKE"
    "EXIT"
    "WAIT"
    "TRIGGER"
    "!"                                 ; Placeholder for positions.
    ))

(defconst krl-types
  '(
    "CHAR"
    "INT"
    "REAL"
    "AXIS"
    "E6AXIS"
    "POS"
    "E6POS"
    "MODUS_T"
    "STATE_T"
    ))

(defconst krl-constants
  '(
    "TRUE"
    "FALSE"
    "$NULLFRAME"
    "$ROBROOT"
    "XHOME"
    ))

;; TODO: Kinda flexible about what we consider builtins...
(defconst krl-builtins
  '(
                                        ; Motion
    "BAS"
    "PTP"
    "PTP_REL"
    "LIN"
    "CIRC"
    "SPL"
    "SPTP"
    "SLIN"
    "SCIRC"
                                        ; Num functions
    "ABS"
    "SQRT"
    "SIN"
    "COS"
    "TAN"
    "ACOS"
    "ATAN2"
                                        ; String functions
    "StrDeclLen"
    "StrLen"
    "StrClear"
    "StrAdd"
    "StrFind"
    "StrComp"
    "StrCopy"
    "SWRITE"
    "SREAD"
                                        ; Magic
    "CWRITE"
    "CREAD"
    ))

(defconst krl-font-lock-keywords
  (list
   (cons (concat "\\<" (regexp-opt krl-keywords t) "\\>") 'font-lock-keyword-face)
   (cons (concat "\\<" (regexp-opt krl-types t) "\\>") 'font-lock-type-face)
   (cons (concat "\\<" (regexp-opt krl-warning-keywords t) "\\>") 'font-lock-warning-face)
   (cons (concat "\\<" (regexp-opt krl-constants t) "\\>") 'font-lock-constant-face)
   '("\\(#\\w*\\)" . font-lock-constant-face)  ; Enum constants
   (cons (concat "\\<" (regexp-opt krl-builtins t) "\\>") 'font-lock-builtin-face)
   '("\\($\\w*\\)" . font-lock-variable-name-face)  ; System variables
   )
  "Highlighting expressions for KRL mode")

(defvar krl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?& "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for krl-mode")

;; Get the string pair for the block we are currently on the end of.
(defun krl-get-pair ()
  (let ((pairs (copy-sequence krl-keywords-nested-block-pairs)) (pair nil))
      (while (and (not pair) pairs)
        (when (looking-at (concat "^ *" (nth 1 (car pairs))))
          (setq pair (car pairs)))
        (setq pairs (cdr pairs)))
      ;; FIXME: Hack for broken ELSE, CASE.
      (when (not pair)
        (let ((triples (copy-sequence krl-keywords-nested-block-middle)))
          (while (and (not pair) triples)
            (when (looking-at (concat "^ *" (nth 2 (car triples))))
              (setq pair (car triples)))
            (setq triples (cdr triples))
          )))
      pair))

(defun krl-indent-of-matching ()
  (save-excursion
    (let ((balance 1) (pair (krl-get-pair)))
      ;(message (nth 0 pair))
      (while (and (/= balance 0) (not (bobp)))
        (progn
          (forward-line -1)
          (if (looking-at (concat "^ *" (nth 0 pair)))
              (setq balance (1- balance)))
          (if (looking-at (concat "^ *" (nth 1 pair)))
              (setq balance (1+ balance)))
          ))
      (current-indentation)
      )))

(defun krl-indent-of-block-opener ()
  (save-excursion
    (let (done)
      (while (and (not done) (not (bobp)))
        (forward-line -1)
        (when (looking-at krl-indent-regex-nested-block-open)
          (setq done t))
        (when (looking-at krl-indent-regex-toplevel-block-open)
          (setq done t))
        )
      (current-indentation)
      )))

(defun krl-indent-of-block-end ()
  (save-excursion
    (let (done)
      (while (and (not done) (not (bobp)))
        (forward-line -1)
        (when (looking-at " *END.*")
          (setq done t))
        )
      (current-indentation)
      )))

(defun krl-find-hinted-indent ()
  (save-excursion
    (let ((ind))
      (while (and (not ind) (not (bobp)))
        (forward-line -1)
        (cond
         ((looking-at krl-indent-regex-toplevel-block-close)
          ;(message "found tlb close")
          (setq ind 0))
         ((looking-at krl-indent-regex-toplevel-block-open)
          ;(message "found tlb open")
          (setq ind (+ 2 (current-indentation))))
         ((looking-at krl-indent-regex-nested-block-close)
          ;(message "found nb close")
          (setq ind (current-indentation)))
         ((looking-at krl-indent-regex-nested-block-open)
          ;(message "found nb open")
          (setq ind (+ 2 (current-indentation))))
         (t
          ;(message "found other")
          )
         ))
      ind)))


(defun indent-and-position (col)
  (progn
    (save-excursion
      (indent-line-to col))
    (when (< (current-column) col)
        (move-to-column col))))


(defun krl-indent-line ()
  "Indent current line as KRL."
  (interactive)
  (let ((ind nil))
  (save-excursion
    (progn
      (beginning-of-line)
      (if (bobp)
          (indent-line-to 0)
        (let (indent-curr)
          (progn
            (cond ((looking-at krl-indent-regex-toplevel-block-open)
                   (progn
                     ;(message "toplevel open")
                     (setq ind 0)))
                  ((looking-at krl-indent-regex-toplevel-block-close)
                   (progn
                     ;(message "toplevel close")
                     (setq ind 0)))
                  ((looking-at "^ *&")
                   (progn
                     ;(message "&")
                     ;; HMI editor crud.
                     (setq ind 0)))
                  ((looking-at krl-indent-regex-nested-block-close)
                   (progn
                     ;(message "nested close")
                     (setq ind (krl-indent-of-matching))))
                  ((looking-at krl-indent-regex-nested-block-middle)
                   (progn
                     ;(message "annoying stuff")
                     ;; (indent-line-to (krl-indent-of-block-opener))))
                     (setq ind (krl-indent-of-matching))))
                  (t
                   (progn
                     ;(message "other")
                     ;; ;(message krl-indent-regex-toplevel-block-open)
                     ;; (indent-line-to (krl-find-hinted-indent))))
                     (let ((hinted-indent (krl-find-hinted-indent)))
                       (when (not hinted-indent)
                         (setq hinted-indent 0))
                       (setq ind hinted-indent))))
            ))))))
  (indent-and-position ind)))


(defun krl-mode ()
  "Major mode for editing KRL files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table krl-mode-syntax-table)
  (use-local-map krl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(krl-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'krl-indent-line)
  (setq major-mode 'krl-mode)
  (setq mode-name "KRL")
  (run-hooks 'krl-mode-hook))

(provide 'krl-mode)
