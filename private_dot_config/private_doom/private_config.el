;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "SF Mono" :size 13)
      doom-variable-pitch-font (font-spec :family "SF Pro Display" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Workspace/org")
(setq org-roam-directory "~/Workspace/org/roam")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; projectile
(setq projectile-project-search-path '("~/Workspace/code"))


;; vterm
(after! vterm
  ;; Don't clear the buffer when switching back to it
  (setq vterm-clear-buffer-on-reenter nil)

  ;; Increase the number of saved scrollback lines
  (setq vterm-max-scrollback 10000))


;; claude code
(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools


;; company
(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2))


;; Permanently display workspaces *in the tab-bar*
;; https://discourse.doomemacs.org/t/permanently-display-workspaces-in-the-tab-bar/4088
(after! persp-mode
  ;; alternative, non-fancy version which only centers the output of +workspace--tabline
  (defun workspaces-formatted ()
    (+doom-dashboard--center (frame-width) (+workspace--tabline)))

  (defun hy/invisible-current-workspace ()
    "The tab bar doesn't update when only faces change (i.e. the
current workspace), so we invisibly print the current workspace
name as well to trigger updates"
    (propertize (safe-persp-name (get-current-persp)) 'invisible t))

  (customize-set-variable 'tab-bar-format '(workspaces-formatted tab-bar-format-align-right hy/invisible-current-workspace))

  ;; don't show current workspaces when we switch, since we always see them
  (advice-add #'+workspace/display :override #'ignore)
  ;; same for renaming and deleting (and saving, but oh well)
  (advice-add #'+workspace-message :override #'ignore))

;; need to run this later for it to not break frame size for some reason
(run-at-time nil nil (cmd! (tab-bar-mode +1)))


;; LSP Configuration with full UI features
(after! lsp-mode  ; Doom macro that runs this config only after lsp-mode loads
  ;; Performance optimizations
  (setq lsp-idle-delay 0.1                    ; Wait 0.1 seconds before sending requests (faster response)
        lsp-log-io nil                        ; Disable LSP communication logging (improves performance)
        lsp-completion-provider :none         ; Let Doom's completion handle completions instead of LSP
        lsp-headerline-breadcrumb-enable t    ; Show navigation breadcrumbs at top of buffer
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols) ; Show project path, file name, and current symbols
        lsp-headerline-breadcrumb-enable-symbol-numbers t ; Number the symbols in breadcrumb
        
        ;; Code lens - Show inline info like "5 references", "3 implementations"
        lsp-lens-enable t                     ; Enable code lens functionality
        lsp-lens-place-position 'above-line  ; Place code lens above the relevant line
        
        ;; Inlay hints - Type annotations and parameter names inline
        lsp-inlay-hint-enable t               ; Enable type hints and parameter names inline
        
        ;; Semantic tokens - Better syntax highlighting based on language server analysis
        lsp-semantic-tokens-enable t          ; Enable enhanced syntax highlighting
        lsp-semantic-tokens-honor-refresh-requests t ; Update highlighting when server requests it
        
        ;; Modeline - Show LSP info in the modeline
        lsp-modeline-code-actions-enable t    ; Show available code actions in modeline
        lsp-modeline-diagnostics-enable t     ; Show error/warning count in modeline
        lsp-modeline-workspace-status-enable t ; Show LSP server status in modeline
        
        ;; Signature help - Function signatures while typing
        lsp-signature-auto-activate t         ; Automatically show function signatures while typing
        lsp-signature-render-documentation t) ; Include documentation in signature help
  
  ;; Enable inlay hints for all LSP modes
  (add-hook 'lsp-mode-hook #'lsp-inlay-hints-mode)) ; Automatically enable inlay hints whenever LSP mode starts

(after! lsp-ui
  ;; Sideline configuration - Right side info panel
  (setq lsp-ui-sideline-enable t              ; Enable the sideline (right side info panel)
        lsp-ui-sideline-show-hover t          ; Show hover information in sideline
        lsp-ui-sideline-show-diagnostics t    ; Show errors/warnings in sideline
        lsp-ui-sideline-show-code-actions t   ; Show available code actions
        lsp-ui-sideline-show-symbol t         ; Show symbol information
        lsp-ui-sideline-ignore-duplicate t    ; Don't show duplicate information
        lsp-ui-sideline-delay 0.2             ; Wait 0.2 seconds before showing sideline
        
        ;; Peek configuration - Preview without leaving current file
        lsp-ui-peek-enable t                  ; Enable peek functionality
        lsp-ui-peek-show-directory t          ; Show directory in peek window
        lsp-ui-peek-peek-height 20           ; Height of peek preview window
        lsp-ui-peek-list-width 50            ; Width of peek results list
        
        ;; Doc configuration - Documentation popups
        lsp-ui-doc-enable t                   ; Enable documentation popups
        lsp-ui-doc-position 'at-point        ; Show docs at cursor position
        lsp-ui-doc-delay 0.5                 ; Wait 0.5 seconds before showing docs
        lsp-ui-doc-max-width 100             ; Maximum width of doc popup
        lsp-ui-doc-max-height 20             ; Maximum height of doc popup
        lsp-ui-doc-show-with-cursor nil      ; Don't show docs when cursor moves
        lsp-ui-doc-show-with-mouse t         ; Show docs on mouse hover
        lsp-ui-doc-enhanced-markdown t       ; Better markdown rendering in docs
        
        ;; Flycheck integration - Error checking
        lsp-ui-flycheck-enable t))           ; Integrate with flycheck for error checking

;; Language-specific LSP configurations
(after! lsp-rust  ; Rust-specific settings
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"                           ; Use clippy for better linting
        lsp-rust-analyzer-server-display-inlay-hints t                           ; Enable all inlay hints for Rust
        lsp-rust-analyzer-display-chaining-hints t                               ; Show types in method chains
        lsp-rust-analyzer-display-parameter-hints t                              ; Show parameter names
        lsp-rust-analyzer-display-closure-return-type-hints t                    ; Show closure return types
        lsp-rust-analyzer-display-reborrow-hints t                               ; Show reborrow hints
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")) ; Show lifetime hints except trivial ones

(after! lsp-go  ; Go-specific settings
  (setq lsp-go-analyses '((fieldalignment . t)  ; Enable static analysis checks:
                         (nilness . t)          ; - field alignment
                         (unusedwrite . t)      ; - nil checks
                         (useany . t))          ; - unused writes, any usage
        lsp-go-staticcheck t                    ; Enable staticcheck linter
        lsp-go-use-gofumpt t))                  ; Use gofumpt for formatting (stricter than gofmt)

(after! lsp-javascript  ; JavaScript/TypeScript settings
  (setq lsp-typescript-preferences-include-package-json-auto-imports "on"  ; Include package.json imports in suggestions
        lsp-typescript-suggest-auto-imports t))                            ; Suggest automatic imports

(after! lsp-python-ms  ; Python settings
  (setq lsp-python-ms-auto-install-server t))  ; Automatically install Python language server

;; Go configuration - Fixed to only apply to Go files
(after! go-mode  ; Only load when go-mode is available
  (setq gofmt-command "goimports")  ; Use goimports instead of gofmt for auto-imports
  (add-hook 'go-mode-hook
            (lambda () (add-hook 'before-save-hook 'gofmt-before-save nil 'local))))  ; Make before-save-hook local to Go files only

;; Enhanced LSP keybindings - Comprehensive key mappings for LSP functions
(map! :localleader  ; Use local leader key (`,` in Doom)
      :map (rust-mode-map go-mode-map python-mode-map js-mode-map typescript-mode-map)  ; Apply to specific mode maps
      :desc "LSP" "l" #'lsp-command-map  ; Access full LSP command map
      
      ;; Navigation - Jump to definitions, references, etc.
      :desc "Find definition" "d" #'lsp-find-definition        ; Jump to where symbol is defined
      :desc "Find references" "r" #'lsp-find-references        ; Find all references to symbol
      :desc "Find implementations" "i" #'lsp-find-implementation  ; Find implementations of interface/trait
      :desc "Find type definition" "t" #'lsp-find-type-definition  ; Jump to type definition
      
      ;; Code actions - Refactoring and code improvements
      :desc "Code actions" "a" #'lsp-execute-code-action      ; Show available code actions (refactoring, fixes)
      :desc "Rename symbol" "R" #'lsp-rename                  ; Rename symbol across project
      :desc "Format buffer" "f" #'lsp-format-buffer          ; Format entire buffer
      :desc "Format region" "F" #'lsp-format-region          ; Format selected region
      
      ;; UI toggles - Control LSP UI elements
      :desc "Toggle sideline" "s" #'lsp-ui-sideline-toggle-symbols-info  ; Toggle right side info panel
      :desc "Toggle doc" "h" #'lsp-ui-doc-toggle             ; Toggle documentation popup
      :desc "Toggle inlay hints" "H" #'lsp-inlay-hints-mode  ; Toggle type hints and parameter names
      
      ;; Peek functions - Preview without leaving current file
      :desc "Peek definition" "p d" #'lsp-ui-peek-find-definitions      ; Preview definition in popup
      :desc "Peek references" "p r" #'lsp-ui-peek-find-references       ; Preview references in popup
      :desc "Peek implementation" "p i" #'lsp-ui-peek-find-implementation)  ; Preview implementations in popup

;; Global LSP keybindings - Access from anywhere
(map! :leader
      :desc "LSP workspace" "c l" #'lsp-command-map)  ; Global access to LSP commands via SPC c l


;; org-roam-ui
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
