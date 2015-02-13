(defvar krl-mode-hook nil)
(defvar krl-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for KRL major mode")

(add-to-list 'auto-mode-alist '("\\.src\\'" . krl-mode))
(add-to-list 'auto-mode-alist '("\\.dat\\'" . krl-mode))
(add-to-list 'auto-mode-alist '("\\.sub\\'" . krl-mode))

(defconst krl-keywords
  '(
    "DEF"
    "END"
    "DEFDAT"
    "ENDDAT"
    "DEFFCT"
    "ENDFCT"
    "DECL"
    "RETURN"
    "FOR"
    "TO"
    "ENDFOR"
    "WHILE"
    "ENDWHILE"
    "IF"
    "THEN"
    "ELSE"
    "ENDIF"
    "SWITCH"
    "CASE"
    "ENDSWITCH"
    "LOOP"
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
                                        ; TODO: Add more stuff.
    ))

(defconst krl-warning-keywords
  '(
    "HALT"
    "BRAKE"
    "EXIT"
    "WAIT"
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
    ))

(defconst krl-constants
  '(
    "TRUE"
    "FALSE"
    "$NULLFRAME"
    "$ROBROOT"
    ))

(defconst krl-builtins
  '(
                                        ; Motion
    "BAS"
    "PTP"
    "PTP_REL"
    "LIN"
    "CIRC"
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

(defun krl-mode ()
  "Major mode for editing KRL files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table krl-mode-syntax-table)
  (use-local-map krl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(krl-font-lock-keywords))
  (setq major-mode 'krl-mode)
  (setq mode-name "KRL")
  (run-hooks 'krl-mode-hook))

(provide 'krl-mode)
