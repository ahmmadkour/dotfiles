;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)

;; Keep cache file outside the config directory ~/.config/emacs_vanilla
;; Must be set before loading no-littering!
;; Can be set on the cli with `--init-directory <path>`
(setq user-emacs-directory "~/.cache/emacs_vanilla")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;(setq use-package-always-defer t)
;(setq use-package-verbose t)

;; Needed for the general package
(require 'help)

(use-package ripgrep :defer t)

;;(use-package auto-package-update
;;  :custom
;;  (auto-package-update-interval 7)
;;  (auto-package-update-prompt-before-update t)
;;  (auto-package-update-hide-results t)
;;  :config
;;  (auto-package-update-maybe)
;;  (auto-package-update-at-time "09:00"))

(use-package no-littering)

(setq backup-directory-alist `(("." . ,(no-littering-expand-var-file-name "backup/")))
      auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      backup-by-copying t                 ; avoid hardlink issues
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)                  ; numbered backups

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defvar my/default-font-size 140)
(defvar my/default-variable-font-size 140)

(set-face-attribute 'default nil :font "Fira Code Retina" :height my/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height my/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height my/default-variable-font-size :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "s-f") #'swiper)

(defun my/open-config-dir ()
  "Open file picker in my Emacs config directory."
  (interactive)
  (let ((default-directory (expand-file-name "~/.config/emacs_vanilla/")))
    (call-interactively (if (fboundp 'counsel-find-file)
                          #'counsel-find-file
                        #'find-file))))

(defun my/open-roam-dir ()
  "Open file picker in my Emacs config directory."
  (interactive)
  (let ((default-directory (expand-file-name "~/Workspace/org/roam")))
    (call-interactively (if (fboundp 'counsel-find-file)
                          #'counsel-find-file
                        #'find-file))))

(defun my/org-roam-search ()
  "Ripgrep search in Org-roam notes with Ivy results."
  (interactive)
  (let ((default-directory (expand-file-name "~/Workspace/org/roam")))
    (counsel-rg "" default-directory)))

(use-package general
  :after evil
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my/leader-keys
    "h" '(:keymap help-map :which-key "help")

    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "."  'find-file

    "f"  '(:ignore t :which-key "files")
    "fs"  '(save-buffer :which-key "save buffer")

    "fp" '(:ignore t :which-key "emacs config")
    "fpo" '(my/open-config-dir :which-key "open config dir")
    "fpt" '(org-babel-tangle :which-key "tangle config file")

    "b" '(:ignore t :which-key "buffer")
    "b[" '(previous-buffer :which-key "previous buffer")
    "b]" '(next-buffer :which-key "next buffer")
    "bB" '(switch-to-buffer :which-key "switch buffer")
    "bd" '(kill-current-buffer :which-key "kill buffer")
    "bs" '(save-buffer :which-key "save buffer")

    "n" '(:ignore t :which-key "notes")

    "nr" '(:ignore t :which-key "roam")
    "nrn" '(org-roam-capture :which-key "Capture to node")
    "nrf" '(org-roam-node-find :which-key "Find node")
    "nri" '(org-roam-node-insert :which-key "Insert node")
    "nrr" '(org-roam-buffer-toggle :which-key "Toggle roam buffer")
    "nf" '(my/open-roam-dir :which-key "Find file in notes")
    "ns" '(my/org-roam-search :which-key "Search notes")

    "p" '(:ignore t :which-key "project")
    "pp" '(projectile-persp-switch-project :which-key "Switch project")
    "pf" '(projectile-find-file :which-key "Find file in project")
    "pa" '(projectile-add-known-project :which-key "Add new project")
    "pd" '(projectile-remove-known-project :which-key "Remove new project")

    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "Magit status")

    "o" '(:ignore t :which-key "open")
    "ot" '(vterm-toggle :which-key "toggle vterm")
    "oT" '(my/vterm-new :which-key "open vterm")
    "o-" '(dired-jump :which-key "Dired")

    "w" '(:keymap evil-window-map :which-key "windows")))
    
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(setq ns-alternate-modifier 'meta) ; left Option = Meta
(setq ns-right-alternate-modifier 'none) ; right Option = literal Alt (# on Opt-3)

(use-package doom-themes
  :ensure t
  :init 
  (load-theme 'doom-one t)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improve) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons) 

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish 
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;; Use information on M-x commands
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; Providing versions of common Emacs commands that are customized to make the best use of Ivy.
(use-package counsel
  :demand t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1) ;; sorting remembered across sessions!
  (ivy-prescient-mode 1))

;; Better formatted help documentation
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(with-eval-after-load 'helpful
  (general-define-key
   :states '(normal visual)
   "K" #'helpful-at-point))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(my/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun my/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (text-scale-set 1))

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/Workspace/org/Tasks.org"
          "~/Workspace/org/Habits.org"
          "~/Workspace/org/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Workspace/org/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Workspace/org/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Workspace/org/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Workspace/org/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Workspace/org/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (my/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Automatically tangle our Emacs.org config file when we save it
(defun my/org-babel-tangle-config ()
  "Tangle the Emacs config file if it's the one being saved."
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs_vanilla/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'after-save-hook #'my/org-babel-tangle-config)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags}" 'face 'org-tag)))
  :custom
  (org-roam-directory "~/Workspace/org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(defun my/persp-switch-to-1 ()
  (interactive)
  (persp-switch-by-number 1))

(defun my/persp-switch-to-2 ()
  (interactive)
  (persp-switch-by-number 2))

(defun my/persp-switch-to-3 ()
  (interactive)
  (persp-switch-by-number 3))

(defun my/persp-switch-to-4 ()
  (interactive)
  (persp-switch-by-number 4))

(defun my/persp-switch-to-5 ()
  (interactive)
  (persp-switch-by-number 5))

(defun my/persp-switch-to-6 ()
  (interactive)
  (persp-switch-by-number 6))

(defun my/persp-switch-to-7 ()
  (interactive)
  (persp-switch-by-number 7))

(defun my/persp-switch-to-8 ()
  (interactive)
  (persp-switch-by-number 8))

(defun my/persp-switch-to-9 ()
  (interactive)
  (persp-switch-by-number 9))

(defun my/persp-switch-to-0 ()
  (interactive)
  (persp-switch-by-number 0))

(use-package perspective
  :ensure t  ; use `:straight t` if using straight.el!
  :general
  ("C-c P C" 'persp-kill-buffer*
   "s-{"     'persp-prev
   "s-}"     'persp-next
   "s-1"     'my/persp-switch-to-1
   "s-2"     'my/persp-switch-to-2
   "s-3"     'my/persp-switch-to-3
   "s-4"     'my/persp-switch-to-4
   "s-5"     'my/persp-switch-to-5
   "s-6"     'my/persp-switch-to-6
   "s-7"     'my/persp-switch-to-7
   "s-8"     'my/persp-switch-to-8
   "s-9"     'my/persp-switch-to-9
   "s-0"     'my/persp-switch-to-0)
  :config
  (my/leader-keys
    "," '(persp-counsel-switch-buffer :which-key "persp switch buffer"))
  :init
  (setq persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-mode))

(with-eval-after-load 'perspective
  (general-define-key
    :states '(normal visual motion)
    :prefix "SPC"
    "TAB" '(:keymap perspective-map :which-key "perspectives")))

(defun my/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . my/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  (setq lsp-completion-provider :none)
  :config
  (lsp-enable-which-key-integration t))

(with-eval-after-load 'lsp-mode
  (general-define-key
   :states '(normal visual)
   :keymaps 'lsp-mode-map
   "K" #'lsp-describe-thing-at-point))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; lsp-ui-doc-enable nil      ; or keep, set lsp-ui-doc-delay
  ;; lsp-ui-sideline-enable nil ; or set lsp-ui-sideline-show-diagnostics t/f
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package lsp-pyright
  :if (executable-find "pyright-langserver")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))) ; or lsp
  :custom
  (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  (lsp-pyright-python-executable-cmd "python3")
  ;; Optional trims:
  ;; (lsp-pyright-use-library-code-for-types t)
  ;; (lsp-pyright-typechecking-mode "basic") ; or "strict"/"off"
  )

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3"))
  ;; (dap-python-executable "python3")
  ;;(dap-python-debugger 'debugpy)
  ;;:config
  ;;(require 'dap-python))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (before-save . gofmt-before-save))
  :config
  (setq gofmt-command "goimports"))  ;; gofmt replacement that also fixes imports

;; Install tools: rustup toolchain install stable
;;                rustup component add rust-analyzer rustfmt clippy
;;                # or standalone rust-analyzer in PATH

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-lsp-client 'lsp-mode          ;; use lsp-mode
        rustic-format-on-save t              ;; rustfmt on save
        lsp-rust-analyzer-server-command '("rust-analyzer")
        lsp-rust-analyzer-cargo-watch-command "clippy" ; optional: lint-on-save
        lsp-rust-analyzer-proc-macro-enable t)
  :hook (rustic-mode . lsp-deferred))

(use-package yaml-mode
:mode "\\.ya?ml\\'"
:hook (yaml-mode . lsp-deferred))

(editorconfig-mode 1)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  (projectile-load-known-projects)
  ;;:custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Workspace/code")
    (setq projectile-project-search-path '("~/Workspace/code")))
  (setq projectile-switch-project-action #'projectile-find-file))

(use-package persp-projectile
  :after (perspective projectile)
  :config
  ;; Use this instead of `projectile-switch-project'
  (define-key projectile-command-map (kbd "p")
	      #'projectile-persp-switch-project))

(use-package magit
   :commands (magit-status magit-get-current-branch)
   :custom
   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Delimiters mode
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000)

  (defun my/vterm-new ()
    "Create a new vterm buffer in the current window."
    (interactive)
    (let ((display-buffer-alist nil))  ; Temporarily disable display rules
      (vterm t))))

  ;;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(with-eval-after-load 'vterm
  ;;  (evil-set-initial-state 'vterm-mode 'emacs)))

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)

  ;; Custom function that ensures bottom display
  (defun my/vterm-toggle-bottom ()
    "Toggle vterm at bottom of screen."
    (interactive)
    (let ((display-buffer-alist
           '((".*"
              (display-buffer-reuse-window display-buffer-at-bottom)
              (window-height . 0.3)))))
      (vterm-toggle))))

(use-package dired
  :ensure nil
  :init
  (setq dired-kill-when-opening-new-dired-buffer t)
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map (kbd "SPC") nil))) ;; clear SPC keybinding in dired so my/leader-keys works

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;;(use-package dired-open
;;  :after dired
;;  :config
;;  ;; Doesn't work as expected!
;;  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
;;  (setq dired-open-extensions '(("png" . "feh")
;;                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 10 1000 1000))
