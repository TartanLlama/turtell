;A basic major mode for the Turtell programming language, based on the tutorial found here:
;http://www.emacswiki.org/emacs/ModeTutorial

(defvar turtell-mode-hook nil)

(defvar turtell-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "{") 'insert-newline-and-indent)
    (define-key map (kbd "}") 'insert-and-indent)
    map)
  "Keymap for Turtell major mode")

(defun insert-newline-and-indent ()
  (interactive)
  (progn
    (self-insert-command 1)
    (newline-and-indent)))

(defun insert-and-indent ()
  (interactive)
  (progn
    (self-insert-command 1)
    (move-beginning-of-line nil)
    (when (looking-at "^[ \t].+}")
	(progn
	  (move-end-of-line nil)
	  (backward-char)
	  (newline)))
    (indent-according-to-mode)
    (forward-char)
    (newline)
    (indent-according-to-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tl\\'" . turtell-mode))

(defconst turtell-font-lock-keywords
  (list
   '("\\<\\(fd\\|forward\\|rt\\|right\\|lt\\|left\\|pu\\|pd\\|penUp\\|penDown\\|chc\\|changeColour\\|mv\\|move\\|if\\|else\\|for\\|while\\|pi\\|sin\\|cos\\|tan\\|arcsin\\|arccos\\|arctan\\|main\\|rand\\|round\\|floor\\|toString\\|length\\)\\>" . font-lock-builtin-face)
   '("\\<\\(\\(+\\|-\\|*\\|/\\|=\\)\\>" . font-lock-operator-face))
  "Highlighting expressions for Turtell mode")

(defvar turtell-font-lock-keywords turtell-font-lock-keywords
  "Default highlighting expressions for WPDL mode")

(setq turtell-tab-width 4)

(defun turtell-indent-line ()
  "Indent current line as Turtell code"
  (interactive)
  (beginning-of-line)
  
  (if (bobp)  ;beginning of buffer
      (indent-line-to 0)
    
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*}") ; Check for rule 2
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) turtell-tab-width)))
	    
	    (if (< cur-indent 0)
		(setq cur-indent 0)))
	
	(save-excursion 
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*}") ;Check if there was a close on the previous line
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
	      (if (looking-at "^[ \t]*.*?{") ;If there is an open
                  (progn
                    (setq cur-indent (+ (current-indentation) turtell-tab-width))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      
      (if cur-indent
          (indent-line-to cur-indent)
	(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(defvar turtell-mode-syntax-table
  (let ((st (make-syntax-table)))

    (modify-syntax-entry ?_ "w" st) ;Words can contain underscores
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for turtell-mode")

(defun turtell-mode ()
  "Major mode for editing Turtell source files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table turtell-mode-syntax-table)
  (use-local-map turtell-mode-map)

  (set (make-local-variable 'font-lock-defaults) '(turtell-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'turtell-indent-line) 

  (setq major-mode 'turtell-mode)
  (setq mode-name "Turtell")
  (run-hooks 'turtell-mode-hook))

(provide 'turtell-mode)