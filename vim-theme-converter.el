;; vim -c ":redir @b
;; :highlight
;; :redir END
;; :execute 'normal \"bp'
;; :w ~/Desktop/test_colvim.txt
;; :qa\!"

(defvar face-name-list '(("Normal" "default")
                         ("Cursor" "cursor")
                         ("CursorLine" "highline-face")
                         ("Visual" "region")
                         ("StatusLine"
                          "mode-line" "mode-line-buffer-id" "minibuffer-prompt")
                         ("StatusLineNC" "mode-line-inactive")
                         ("LineNr" "linum"
                          "fringe")
                         ("MatchParen"  "show-paren-match-face")
                         ("Search" "isearch")
                         ("IncSearch" "isearch-lazy-highlight-face")
                         ("Comment" "font-lock-comment-face"
                          "font-lock-doc-face")
                         ("Statement" "font-lock-builtin-face")
                         ("Function" "font-lock-function-name-face")
                         ("Keyword" "font-lock-keyword-face")
                         ("String" "font-lock-string-face")
                         ("Type" "font-lock-type-face")
                         ("Identifier" "font-lock-variable-name-face")
                         ("Constant" "font-lock-constant-face")
                         ("Error" "font-lock-warning-face")
                         ("PreProc" "font-lock-preprocessor-face")
                         ("Underlined" "underline")
                         ("Directory" "dired-directory")
                         ("Pmenu" "ac-candidate-face")
                         ("PmenuSel" "ac-selection-face")
                         ("SpellBad" "flyspell-incorrect")
                         ("SpellRare" "flyspell-duplicate")))

(defun vimco-get-colors (search-str)
  (lexical-let ((attr-list '("guifg=" "guibg=" "gui=" ))
                (color-list ())
                (result-list ())
                (start-point 0))
    (beginning-of-buffer)
    (setq attr-list (append attr-list (list (concat "link +\\<" search-str "\\> +"))))
    (while (re-search-forward (concat "[^\"] *\\<hi\\> +\\(\\<link\\> +\\)?\\<" search-str "\\>") nil t)
      (dolist (var attr-list color-list)
        (beginning-of-line)
        (setq start-point (re-search-forward var (line-end-position) t))
        (if (not (number-or-marker-p start-point))
            (push '(()) color-list)  
          (if (/= start-point 0)
              (progn
                ;; extract color string
                (setq end-point (+ (skip-chars-forward "a-zA-Z#,0-9" (line-end-position)) start-point))
                (setq color-string (buffer-substring  start-point end-point))
                (if (or (string= "" color-string) (string= color-string "none"))
                    (push '(()) color-list)
                  (push (split-string color-string "[, ]" t) color-list))))))
      (push (reverse color-list) result-list)
      (setq color-list ())
      (end-of-line))
    result-list))

(defun vimco-build-face-list ()
  (lexical-let (
                (face-attributes-list ())
                (fg-color)
                (bg-color)
                (style)
                (inherit)
                (result-str "")
                (comment-alternatives nil))
    (beginning-of-buffer)
    (dolist (face face-name-list)
      (setq face-attributes-list (vimco-get-colors (car face)))
      (dolist (face-attributes face-attributes-list)
        (setq fg-color (car (nth 0 face-attributes)))
        (setq bg-color (car (nth 1 face-attributes)))
        (setq style (nth 2 face-attributes))
        (setq inherit (car (nth 3 face-attributes)))
        (setq color-style-str (concat
                               " ((t ("
                               (if fg-color
                                   (concat ":foreground " "\"" fg-color "\" "))
                               (if bg-color
                                   (concat ":background " "\"" bg-color "\" "))
                               (if (car style)
                                   (dolist (var style color-style-str)
                                     (if (string= var "bold")
                                         (setq color-style-str (concat color-style-str ":weight bold "))
                                       (setq color-style-str (concat color-style-str ":" var " t " )))))
                               (if inherit
                                   (progn
                                     (print inherit)
                                     (concat ":inherit " (cadr (assoc inherit face-name-list)))))
                               "))))\n"))
        (dolist (emacs-face (cdr face) result-str)
          (setq result-str (concat result-str
                                   (if comment-alternatives ";; ")
                                   "'(" emacs-face color-style-str)))
        (setq color-style-str "")
        (setq comment-alternatives t))
      (setq comment-alternatives nil))
    result-str))

(defun vimco-convert-theme ()
  (interactive)
  (lexical-let ((theme-name "")
                (faces-list-str ""))
    (setq faces-list-str (vimco-build-face-list))
    (print faces-list-str)

    (setq theme-name (car (split-string (buffer-name) "\\.")))
    ;; open new buffer
    (set-buffer (get-buffer-create (concat theme-name ".el")))
    (insert (concat "\n(deftheme " theme-name " \"\")\n\n"))
    (insert (concat "(custom-theme-set-faces\n '" theme-name "\n"))
    (insert faces-list-str)
    (insert ")\n\n")
    (insert (concat "(provide-theme '" theme-name ")\n"))
    (normal-mode)
    (lisp-mode)))

(defun test-vimco ()
  (interactive)
  (print (vimco-get-colors "VertSplit")))

(defun vimco-use-vim ()
  (interactive)
  (lexical-let ((temp-file))
    (setq temp-file  (concat temporary-file-directory "vim-colors.txt"))
    (print temp-file)
    (call-process "vim" nil nil nil (concat "-c :set columns=300
:redir @b
:highlight
:redir END
:execute 'normal \"bp'
:w! " temp-file
"
:qa"))
    (find-file temp-file)
    ))

;; (concat "-c :set columns=300
;; :redir @b
;; :highlight
;; :redir END
;; :execute 'normal \"bp'
;; :w! " temp-file
;; "
;; :qa")
