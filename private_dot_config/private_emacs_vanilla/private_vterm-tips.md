# Vterm Tips & Improvements

## Current Setup (init.el lines 1185-1214)

- Shell: zsh
- Scrollback: 10000 lines
- `vterm-toggle` for quick access
- Keybindings: `SPC o t` (toggle) and `SPC o T` (new)

## Improvements to Config

```elisp
(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t)        ; auto-close buffer on exit
  (setq vterm-copy-mode-remove-fake-newlines t) ; cleaner copy

  ;; Clear scrollback with C-c C-l (like terminal clear)
  (define-key vterm-mode-map (kbd "C-c C-l")
    (lambda () (interactive) (vterm-clear-scrollback) (vterm-clear)))

  (defun my/vterm-new ()
    "Create a new vterm buffer in the current window."
    (interactive)
    (let ((display-buffer-alist nil))
      (vterm t))))
```

## Essential Shortcuts

| Key | Action |
|-----|--------|
| `C-c C-t` | Enter **copy mode** (use vim motions to select text) |
| `RET` (in copy mode) | Copy selection and exit copy mode |
| `C-c C-n` / `C-c C-p` | Next/previous prompt (if shell integration enabled) |
| `C-y` | Paste from kill ring |
| `C-c C-c` | Send interrupt (like terminal Ctrl-C) |
| `C-c C-z` | Send suspend |

## Shell Integration (Highly Recommended)

Add this to `~/.zshrc` for better navigation:

```bash
# vterm shell integration
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    vterm_printf() { printf "\e]%s\e\\" "$1" }

    # Directory tracking - Emacs knows your pwd
    vterm_prompt_end() { vterm_printf "51;A$(whoami)@$(hostname):$(pwd)" }
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

    # Open files in Emacs from terminal
    vterm_cmd() {
        local vterm_elisp
        vterm_elisp=""
        while [ $# -gt 0 ]; do
            vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
            shift
        done
        vterm_printf "51;E$vterm_elisp"
    }

    # Usage: `ff filename` opens file in Emacs
    ff() { vterm_cmd find-file "$(realpath "${@:-.}")" }

    # Usage: `man ls` opens man page in Emacs
    man() { vterm_cmd man "$@" }
fi
```

## Supporting Packages

### multi-vterm - Better multi-terminal management

```elisp
(use-package multi-vterm
  :after vterm
  :bind (("C-c t t" . multi-vterm)
         ("C-c t n" . multi-vterm-next)
         ("C-c t p" . multi-vterm-prev)
         ("C-c t d" . multi-vterm-dedicated-toggle)))
```

## Productivity Helpers

### Project-scoped terminals

```elisp
(defun my/vterm-in-project ()
  "Open vterm in project root."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (vterm t)))
```

### Quick directory sync - Jump vterm to current buffer's directory

```elisp
(defun my/vterm-cd-here ()
  "Send cd command to vterm for current buffer's directory."
  (interactive)
  (let ((dir (if buffer-file-name
                 (file-name-directory buffer-file-name)
               default-directory)))
    (if-let ((vterm-buf (seq-find (lambda (b)
                                    (eq 'vterm-mode (buffer-local-value 'major-mode b)))
                                  (buffer-list))))
        (with-current-buffer vterm-buf
          (vterm-send-string (format "cd %s\n" (shell-quote-argument dir))))
      (message "No vterm buffer found"))))
```

### Fix vterm-toggle display at bottom

Your `my/vterm-toggle-bottom` isn't being used - the keybinding uses `vterm-toggle` directly. Either update the keybinding or configure `vterm-toggle` with display rules:

```elisp
(add-to-list 'display-buffer-alist
             '("\\*vterm\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)))
```

## Copy Mode Workflow

1. Press `C-c C-t` to enter copy mode
2. Use vim motions (`/` to search, `v` to select, `hjkl` to move)
3. Press `RET` to copy selection and exit

## Tips

- Use shell integration for directory tracking - Emacs will always know your pwd
- The `ff` shell function lets you open files from terminal directly in Emacs
- `multi-vterm` is useful if you frequently work with multiple terminals
- Consider binding `my/vterm-in-project` to `SPC o p` for project-scoped terminals
