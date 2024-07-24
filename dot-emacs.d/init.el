;;; init.el --- VN -*- lexical-binding: t; -*-

;; fix org-modern fonts
;; denote https://protesilaos.com/emacs/denote
;; eldoc box? eldoc eager https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc https://github.com/casouri/eldoc-box
;; nicer calc https://github.com/sulami/literate-calc-mode.el http://yummymelon.com/devnull/announcing-casual-an-opinionated-porcelain-for-emacs-calc.html
;; personalize keybinds
;; debugger https://github.com/realgud
;; dired https://github.com/alexluigit/dirvish https://github.com/Fuco1/dired-hacks http://yummymelon.com/devnull/announcing-casual-dired---an-opinionated-porcelain-for-the-emacs-file-manager.html
;; color-moccur
;; update org to 9.7? https://github.com/minad/org-modern/discussions/195
;; dabbrev? hippie? https://www.masteringemacs.org/article/text-expansion-hippie-expand
;; customize eat https://emacsconf.org/2023/talks/eat/
;; replace org-timeblock https://github.com/dmitrym0/org-hyperscheduler/ https://github.com/alphapapa/org-super-agenda

(use-package emacs
  :init
;;; global defuns
  (defun reload-config ()
    "Reloads init.el"
    (interactive)
    (load-file user-init-file))

  (defun sudo ()
    "Use TRAMP to `sudo' the current buffer."
    (interactive)
    (when buffer-file-name
      (find-alternate-file
       (concat "/sudo:root@localhost:"
               buffer-file-name))))

  (defmacro k-time (&rest body)
    "Measure and return the time it takes evaluating BODY."
    `(let ((time (current-time)))
       ,@body
       (float-time (time-since time))))

  ;; When idle for 15sec run the GC no matter what.
  (defvar k-gc-timer
    (run-with-idle-timer 15 t
                         (lambda ()
                           (message "Garbage Collector has run for %.06fsec"
                                    (k-time (garbage-collect))))))

  (defun toggle-window-split ()
    "Switch between horizontal and vertical split window layout."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  (defun set-buffer-width (buffer-or-name)
    "Resize the window displaying BUFFER-OR-NAME to about 1/3 of the frame width."
    (let* ((buffer (get-buffer buffer-or-name))
           (window (get-buffer-window buffer)))
      (when window
        (with-selected-window window
          (let ((target-width (/ (frame-width) 3)))
            (when (> (window-width) target-width)
              (shrink-window-horizontally (- (window-width) target-width))))))))

  ;; store common buffers to registers
  (set-register ?S '(buffer . "*scratch*"))
  (set-register ?I `(file . ,(expand-file-name "init.el" user-emacs-directory)))

  ;; C-c bindings
  (global-set-key (kbd "C-c b") #'compile)
  (global-set-key (kbd "C-c B") #'recompile)
  (global-set-key (kbd "C-c c") #'calendar)
  (global-set-key (kbd "C-c f") #'ffap)
  (global-set-key (kbd "C-c s") #'toggle-window-split)

  ;; always install declared packages
  (setq use-package-always-ensure t)
  ;; get updates to builtin packages
  (setq package-install-upgrade-built-in t)
  ;; refreshes package cache if we install anything
  (defvar genehack/packages-refreshed nil
    "Flag for whether package lists have been refreshed yet.")

  (defun genehack/package-refresh (&rest args)
    "Refresh package metadata, if needed.
Ignores `ARGS'."
    (unless (eq genehack/packages-refreshed t)
      (progn
        (package-refresh-contents)
        (setq genehack/packages-refreshed t))))
  (advice-add 'package-install :before #'genehack/package-refresh)

  ;; track recently opened files
  (recentf-mode t)
  (add-hook 'find-file-hook 'recentf-save-list)

  ;; no bell
  (setq ring-bell-function 'ignore)

  ;; start server for emacsclient
  (server-start)

  ;; shorten yes/no answer prompts to y/n
  (setq use-short-answers t)

  ;; Don't hang when visiting files with extremely long lines
  (global-so-long-mode t)


  ;; vertico helper
  (defun crm-indicator (args)
    "Prompt indicator for `completing-read-multiple'.
Appears as [CRM<`crm-separator'>]"
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; don't allow cursor into the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t) ; minibuffs in minibuffs

  ;; "If ‘complete’, TAB first tries to indent the current line, and if the line
  ;; was already indented, then try to complete the thing at point."
  (setq tab-always-indent 'complete)

  ;; omits some inapplicable commands from other modes
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; makes the mark buffer bigger
  (setq-default mark-ring-max 32)
  (setq global-mark-ring-max 32)

  ;; right click shows context menu
  (when (display-graphic-p)
    (context-menu-mode))

  ;; automatically reread from disk if the underlying file changes
  ;; uses fs events - os dependent
  (setq auto-revert-avoid-polling t)
  (setq auto-revert-interval 5)
  (setq auto-revert-check-vc-info t)
  (global-auto-revert-mode)

  ;; store backups in ~/.emacs.d/backups rather than littering
  (defun bedrock/backup-file-name (fpath)
    "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
    (let* ((backupRootDir (concat user-emacs-directory "backups/"))
           (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
           (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
      (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
      backupFilePath))
  (setq make-backup-file-name-function 'bedrock/backup-file-name)

  (setq vc-follow-symlinks t)                      ; no annoying symlink warning
  (setq inhibit-splash-screen t)                   ; no splash screen
  (setq line-number-mode t)                        ; show current line in modeline
  (setq column-number-mode t)                      ; show column as well
  (setq x-underline-at-descent-line nil)           ; prettier underlines
  (setq switch-to-buffer-obey-display-actions t)   ; make switching buffers more consistent
  (blink-cursor-mode -1)                           ; steady cursor
  (global-visual-line-mode)                        ; line wrap at word boundaries
  (pixel-scroll-precision-mode)                    ; smooth scrolling
  (setq-default fill-column 80)
  ;; display line numbers in programming mode
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (setq display-line-numbers-width-start 1)        ; avoids horizontal jitter

  ;; vim-like scrolling behavior
  (setq scroll-margin 5
	scroll-conservatively 101
	scroll-preserve-screen-position t
	)
  (setq pixel-scroll-precision-large-scroll-height 35.0)

  ;; highlight the current line
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode)
  (add-hook 'org-mode-hook #'hl-line-mode)

  ;; spell checker
  (cond ((executable-find "aspell")
         (setq ispell-program-name "aspell"
               ispell-really-aspell t)))

  ;; delete selection on yank
  (delete-selection-mode t)

  ;; pop marks repeatedly after first prefix
  (setq set-mark-command-repeat-pop t)
  ;; And, because I always forget it, to pop a global mark you use C-x C-<SPC>.
  ;; The local version, C-u C-<SPC> will only pop marks from the current buffer.
  ;; So the C-x C-<SPC> version is much closer to how Vim's jump stack works.


  ;; delete trailing whitespace on save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; auto-matching parens
  (electric-pair-mode t)

  ;; don't use tab characters for indents
  (setq-default indent-tabs-mode nil)

  ;; save place in files
  (save-place-mode t)

  ;; set window title
  (setq frame-title-format "%b - emacs")

  ;; view follows output until first error
  (setq compilation-scroll-output 'first-error)

  ;; confirm quitting emacs
  (setq confirm-kill-emacs 'yes-or-no-p)

  ;; hide tab bar until there's more than one tab
  (setq tab-bar-show 1)
  ;; alternate between window layouts in a single frame
  (tab-bar-mode)
  ;; move through layout history
  (tab-bar-history-mode)
  (global-set-key (kbd "M-[") 'tab-bar-history-back)
  (global-set-key (kbd "M-]") 'tab-bar-history-forward)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

  ;; move between windows with S-<arrow>
  (windmove-default-keybindings 'shift)
  ;; alternate window with M-o
  (global-set-key (kbd "M-o") 'other-window)
  ;; use ibuffer for C-x C-b
  (global-set-key [remap list-buffers] 'ibuffer)

;;; font
  (set-face-attribute 'default nil
                      :family "Berkeley Mono"
                      :height 110
                      :weight 'normal
                      :width 'normal)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")

;;; theme
  (use-package modus-themes)
  (defvar vn-light-theme 'modus-operandi
    "The light theme to use.")
  (defvar vn-dark-theme 'modus-vivendi
    "The dark theme to use.")

  ;; sets theme using os appearance (depends on emacs-plus) or location
  (if (and (eq system-type 'darwin) (display-graphic-p))
      (progn (defun my/apply-theme (appearance)
	       "Load theme, taking current system APPEARANCE into consideration."
	       (mapc #'disable-theme custom-enabled-themes)
	       (pcase appearance
	         ('light (load-theme vn-light-theme t))
	         ('dark (load-theme vn-dark-theme t))))
	     (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
	     )
    (progn
      (use-package circadian
        :config
        (setq calendar-latitude 37.0)
        (setq calendar-longitude -122.0)
        (setq circadian-themes `((:sunrise . ,vn-light-theme)
                                 (:sunset  . ,vn-dark-theme)))
        (circadian-setup))
      )
    )
  ;; Completely hide visual-line-mode and change auto-fill-mode to " AF".
  :delight
  (auto-fill-function " AF")
  (visual-line-mode)
  )

;;; packages
;; melpa and nongnu
(use-package package
  :init
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  )

;; pretty eldoc, md editing
(use-package markdown-mode)

(use-package eglot
  ;; Configure hooks to automatically turn-on eglot for selected modes
                                        ; :hook
                                        ; (((python-mode ruby-mode elixir-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
                                        ; (add-to-list 'eglot-server-programs
                                        ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )

;; hide common minor modes
(use-package delight)

;; automatically use treesitter
(use-package treesit-auto
  :init
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/Azganoth/tree-sitter-lua")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (r "https://github.com/r-lib/tree-sitter-r")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (swift "https://github.com/tree-sitter/tree-sitter-swift")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; org mode
(use-package org
  ;; :hook
  ;; ((org-mode . flyspell-mode))
  :bind (:map global-map
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)
              ("C-c o a" . org-agenda)
              ("C-c o b d" . org-babel-detangle)
              ("C-c o b o" . org-babel-tangle-jump-to-org)
              ("C-c o b s" . renz/org-babel-tangle-jump-to-src)
              ("C-c o k" . org-babel-remove-result)
              ("C-c t" . create-heading-with-timestamp)
              ) ; Mnemonic: link → insert

  ;; :custom
  ;;  (org-block ((t (:inherit fixed-pitch))))
  ;;  (org-code ((t (:inherit (shadow fixed-pitch)))))
  ;;  (org-document-info ((t (:foreground "dark orange"))))
  ;;  (org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;;  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  ;;  (org-link ((t (:foreground "royal blue" :underline t))))
  ;;  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  (org-property-value ((t (:inherit fixed-pitch))) t)
  ;;  (org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  (org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  ;;  (org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;;  (org-verbatim ((t (:inherit (shadow fixed-pitch)))))

  :config
  (defun create-heading-with-timestamp ()
    "Create a new heading with an inactive timestamp (including current time) as the title and position cursor below, respecting current heading level."
    (interactive)
    (let* ((current-level (org-current-level))
           (stars (if current-level
                      (make-string current-level ?*)
                    "*")))
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (insert stars " ")
      (org-insert-time-stamp (current-time) t t)
      (insert "\n")
      (setq pos (point))
      (unless (eobp)
        (insert "\n")
        (backward-char))
      (goto-char pos)))
  (setq org-fold-core-style 'overlays)  ; fix folding <-> isearch
  (defun renz/org-babel-tangle-jump-to-src ()
    "The opposite of `org-babel-tangle-jump-to-org'.
Jumps to an Org src block from tangled code."
    (interactive)
    (if (org-in-block-p)
        (let* ((header (car (org-babel-tangle-single-block 1 'only-this-block)))
               (tangle (car header))
               (lang (caadr header))
               (buffer (nth 2 (cadr header)))
               (org-id (nth 3 (cadr header)))
               (source-name (nth 4 (cadr header)))
               (search-comment (org-fill-template
                                org-babel-tangle-comment-format-beg
                                `(("link" . ,org-id) ("source-name" . ,source-name))))
               (file (expand-file-name
                      (org-babel-effective-tangled-filename buffer lang tangle))))
          (if (not (file-exists-p file))
              (message "File does not exist. 'org-babel-tangle' first to create file.")
            (find-file file)
            (beginning-of-buffer)
            (search-forward search-comment)))
      (message "Cannot jump to tangled file because point is not at org src block.")))

  (defun renz/list-files-with-absolute-path (directory)
    "Return a list of org files in DIRECTORY with their absolute paths."
    (cl-remove-if-not #'file-regular-p (directory-files directory t ".*\.org$")))
  (setq org-directory "~/Documents/org/")
  ;; Refile configuration
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-tag-alist '(
                        ;; locale
                        (:startgroup)
                        ("home" . ?h)
                        ("work" . ?w)
                        ("study" . ?s)
                        (:endgroup)
                        (:newline)
                        ;; scale
                        (:startgroup)
                        ("one-shot" . ?o)
                        ("project" . ?j)
                        ("tiny" . ?t)
                        (:endgroup)
                        ;; misc
                        ("meta")
                        ("review")
                        ("reading")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)
  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("w" "Work")
          ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("wr" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   org-image-actual-width '(300)
   org-startup-with-inline-images t
   org-image-actual-width nil   ; allow image resizing

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  (setq org-startup-indented t)

  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos"
           ((agenda)
            (todo)))
          ("w" "Work" agenda ""
           ((org-agenda-files '("work.org"))))))

  ;; proportional headline size
  ;;   (let* ((variable-tuple
  ;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
  ;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
  ;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
  ;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
  ;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
  ;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
  ;;        (base-font-color     (face-foreground 'default nil 'default))
  ;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
  ;;   (custom-theme-set-faces
  ;;    'user
  ;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
  ;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
  ;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
  ;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  )
;; makes markdown elements appear when editing them
(use-package org-appear
  :hook
  (org-mode . org-appear-mode))
(use-package org-modern
  ;; :custom
  ;; (org-modern-keyword nil)
  ;; (org-modern-checkbox nil)
  ;; (org-modern-table nil))
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")
  (global-org-modern-mode))
(use-package olivetti
  :config
  ;; Distraction-free writing
  (defun ews-distraction-free ()
    "Distraction-free writing environment using Olivetti package."
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-set 2)
          (olivetti-mode t))
      (progn
        (if (eq (length (window-list)) 1)
            (jump-to-register 1))
        (olivetti-mode 0)
        (text-scale-set 0))))
  (setq olivetti-style 'fancy)
  (setq olivetti-body-width 97)
  :bind
  (("<f9>" . ews-distraction-free))
  )
(use-package org-alert
  :config
  (setq org-alert-interval 300
	org-alert-notification-title "Reminder"
        org-alert-notify-after-event-cutoff 5)
  (setq alert-default-style 'osx-notifier)
  (setq org-alert-time-match-string
        "\\(?:SCHEDULED\\|DEADLINE\\):.*?<.*?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\(?:-[0-9]\\{2\\}:[0-9]\\{2\\}\\)?.*?>")
  (org-alert-enable)
  )
(use-package org-timeblock)

;; nice git porcelain
(use-package magit
  :ensure t
  :config
  ;; from https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
  (defun th/magit--with-difftastic (buffer command)
    "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
    (let ((process-environment
           (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                         (number-to-string (frame-width)))
                 process-environment)))
      ;; Clear the result buffer (we might regenerate a diff, e.g., for
      ;; the current changes in our working directory).
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer))
      ;; Now spawn a process calling the git COMMAND.
      (make-process
       :name (buffer-name buffer)
       :buffer buffer
       :command command
       ;; Don't query for running processes when emacs is quit.
       :noquery t
       ;; Show the result buffer once the process has finished.
       :sentinel (lambda (proc event)
                   (when (eq (process-status proc) 'exit)
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-min))
                       (ansi-color-apply-on-region (point-min) (point-max))
                       (setq buffer-read-only t)
                       (view-mode)
                       (end-of-line)
                       ;; difftastic diffs are usually 2-column side-by-side,
                       ;; so ensure our window is wide enough.
                       (let ((width (current-column)))
                         (while (zerop (forward-line 1))
                           (end-of-line)
                           (setq width (max (current-column) width)))
                         ;; Add column size of fringes
                         (setq width (+ width
                                        (fringe-columns 'left)
                                        (fringe-columns 'right)))
                         (goto-char (point-min))
                         (pop-to-buffer
                          (current-buffer)
                          `(;; If the buffer is that wide that splitting the frame in
                            ;; two side-by-side windows would result in less than
                            ;; 80 columns left, ensure it's shown at the bottom.
                            ,(when (> 80 (- (frame-width) width))
                               #'display-buffer-at-bottom)
                            (window-width
                             . ,(min width (frame-width))))))))))))
  (defun th/magit-show-with-difftastic (rev)
    "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If REV is given, just use it.
            (when (boundp 'rev) rev)
            ;; If not invoked with prefix arg, try to guess the REV from
            ;; point's position.
            (and (not current-prefix-arg)
                 (or (magit-thing-at-point 'git-revision t)
                     (magit-branch-or-commit-at-point)))
            ;; Otherwise, query the user.
            (magit-read-branch-or-commit "Revision"))))
    (if (not rev)
        (error "No revision specified")
      (th/magit--with-difftastic
       (get-buffer-create (concat "*git show difftastic " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev))))
  (defun th/magit-diff-with-difftastic (arg)
    "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If RANGE is given, just use it.
            (when (boundp 'range) range)
            ;; If prefix arg is given, query the user.
            (and current-prefix-arg
                 (magit-diff-read-range-or-commit "Range"))
            ;; Otherwise, auto-guess based on position of point, e.g., based on
            ;; if we are in the Staged or Unstaged section.
            (pcase (magit-diff--dwim)
              ('unmerged (error "unmerged is not yet implemented"))
              ('unstaged nil)
              ('staged "--cached")
              (`(stash . ,value) (error "stash is not yet implemented"))
              (`(commit . ,value) (format "%s^..%s" value value))
              ((and range (pred stringp)) range)
              (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
    (let ((name (concat "*git diff difftastic"
                        (if arg (concat " " arg) "")
                        "*")))
      (th/magit--with-difftastic
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))
  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)])
  (transient-append-suffix 'magit-dispatch "!"
    '("#" "My Magit Cmds" th/magit-aux-commands))

  (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands)

  )

;; persist history
(use-package savehist
  :init
  (savehist-mode))

;; startup dashboard
(use-package fortune
  :config
  (defun get-multiple-fortunes (count)
    (let (fortunes)
      (dotimes (_ count)
        (setq fortunes (cons (fortune-message) fortunes)))
      fortunes)
    )
  (if (eq system-type 'darwin)
      (progn
        (setq fortune-dir "/opt/homebrew/Cellar/fortune/9708/share/games/fortunes"
              fortune-file "/opt/homebrew/Cellar/fortune/9708/share/games/fortunes/fortunes"))))
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-image-banner-max-height 300)
  (setq dashboard-image-banner-max-width 300)
  (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-extra-images (if (file-directory-p "~/.emacs.d/decor") (directory-files "~/.emacs.d/decor" t ".png")))
  (setq dashboard-startup-banner (cons 'logo dashboard-extra-images))
  (setq dashboard-vertically-center-content t) ;FIXME doesn't center on startup https://github.com/emacs-dashboard/emacs-dashboard/issues/534
  (setq dashboard-center-content t)
  (setq dashboard-items '((projects  . 5)
                          (agenda    . 5)
                          (recents   . 5)
                          ))
  (setq dashboard-footer-messages (get-multiple-fortunes 7))
  (dashboard-setup-startup-hook))

;; minibuffer completion framework
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 22) ;; show more candidates
  (setq vertico-cycle t) ;; cycle over at bottom/top
  )

;; rich annotations (e.g. docstrings) in the minibuffer
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; terminal emulator
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm")
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

;; buffer completions
(use-package corfu
  :custom
  (corfu-cycle t)     ; cycle over at bottom/top
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-auto-prefix 2)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt

  :bind
  (:map corfu-map
        ;; ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode 1) ; docstrings in buffer completion popups

  ;; fixes behavior in eshell
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode)
              )
	    ))
(use-package corfu-terminal
  :ensure t
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  )

;; templates
(use-package tempel
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )
(use-package tempel-collection
  :ensure t)

;; allows for company backends to be used with corfu
;; (use-package cape
;;   ;; Bind dedicated completion commands
;;   ;; Alternative prefix keys: C-c p, M-p, M-+, ...
;;   :bind (("C-c p p" . completion-at-point) ;; capf
;;          ("C-c p t" . complete-tag)        ;; etags
;;          ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
;;          ("C-c p h" . cape-history)
;;          ("C-c p f" . cape-file)
;;          ("C-c p k" . cape-keyword)
;;          ("C-c p s" . cape-elisp-symbol)
;;          ("C-c p e" . cape-elisp-block)
;;          ("C-c p a" . cape-abbrev)
;;          ("C-c p l" . cape-line)
;;          ("C-c p w" . cape-dict)
;;          ("C-c p :" . cape-emoji)
;;          ("C-c p \\" . cape-tex)
;;          ("C-c p _" . cape-tex)
;;          ("C-c p ^" . cape-tex)
;;          ("C-c p &" . cape-sgml)
;;          ("C-c p r" . cape-rfc1345))
;;   :init
;;   ;; Add to the global default value of `completion-at-point-functions' which is
;;   ;; used by `completion-at-point'.  The order of the functions matters, the
;;   ;; first function returning a result wins.  Note that the list of buffer-local
;;   ;; completion functions takes precedence over the global list.
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-elisp-block)
;;   ;;(add-hook 'completion-at-point-functions #'cape-history)
;;   ;;(add-hook 'completion-at-point-functions #'cape-keyword)
;;   ;;(add-hook 'completion-at-point-functions #'cape-tex)
;;   ;;(add-hook 'completion-at-point-functions #'cape-sgml)
;;   ;;(add-hook 'completion-at-point-functions #'cape-rfc1345)
;;   ;;(add-hook 'completion-at-point-functions #'cape-abbrev)
;;   ;;(add-hook 'completion-at-point-functions #'cape-dict)
;;   ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
;;   ;;(add-hook 'completion-at-point-functions #'cape-line)
;; )

;; better help buffer (but the way it handles history and window placement is worse)
(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h x" . helpful-command)
   ("C-c C-d" . helpful-at-point))
  :init

  (defun helpful-switch-to-buffer (buffer-or-name)
    "Display BUFFER-OR-NAME in a Helpful window.
Reuse existing Helpful windows if possible, otherwise create a new one.
Resize the Helpful window and keep focus in the original window."
    (let* ((helpful-window (get-window-with-predicate
                            (lambda (window)
                              (with-current-buffer (window-buffer window)
                                (eq major-mode 'helpful-mode)))))
           (buffer (get-buffer buffer-or-name))
           (orig-window (selected-window)))
      (if helpful-window
          ;; If a Helpful window exists, use it
          (set-window-buffer helpful-window buffer)
        ;; Otherwise, create a new window
        (select-window (split-window-right (- (window-width) (/ (frame-width) 3))))
        (switch-to-buffer buffer))
      ;; Ensure the Helpful buffer is displayed and sized correctly
      (set-buffer-width buffer)
      ;; Return focus to the original window
      (select-window orig-window)))

  (setq helpful-switch-buffer-function #'helpful-switch-to-buffer)
  )

;; eshell
(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

;; which-key: shows a popup of available keybindings when typing a long key seq
(use-package which-key
  :config
  (which-key-mode)
  :delight)

;; orderless style completion
(use-package orderless
  :custom
  ;; basic completion is kept as a fallback
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; jump around
(use-package avy
  :demand t
  :bind (("C-x j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer))
  )

;; modify results in grep buffers
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; embark w/ consult integration
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark)
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; visualize the undo tree
(use-package vundo
  :commands (vundo)
  ;;:bind ("C-M-u" . vundo) ; TODO find a bind
  )

;; epub reader
(use-package esxml
  :ensure t)
(use-package nov
  :after esxml
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; consult - search and navigate
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

;; diagnostics jumping binds
(use-package flymake
  :bind (:map flymake-mode-map
              ("C-c n" . flymake-goto-next-error)
              ("C-c p" . flymake-goto-prev-error)))

;;; 'workspaces' in this config rely on a combination of tab-bar-mode, project.el and
;;; burly, with consult making nice switching interfaces for everything
;; save windows configurations
(use-package burly
  ;; TODO bind burly-reset-tab, burly-bookmark-windows, burly-open-bookmark, burly-open-last-bookmark
  :config
  (burly-tabs-mode))

;; meow modal editing
(use-package meow
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-global-mode 1)
  (meow-setup-indicator) ;; adds mode-line indicator
  )

;; use ripgrep instead of grep
(use-package grep
  :config
  (when (executable-find "rg")
    (setq grep-program "rg")
    (grep-apply-setting
     'grep-find-command
     '("rg -n -H --color always --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 42))))

;; shorten file paths in grep/compilations buffers
(use-package scf-mode
  :ensure t
  :load-path "site-lisp"
  :hook (grep-mode . (lambda () (scf-mode 1))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
