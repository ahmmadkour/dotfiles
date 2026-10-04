;; Set high GC threshold during startup for faster loading
;; This effectively stop GC; gcmh takes over once init.el loads it.
(setq gc-cons-threshold most-positive-fixnum)

;; Keep packages, caches and state outside the config directory ~/.config/emacs.
;; Must be set before loading no-littering!
;; Can be set on the cli with `--init-directory <path>'
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; package.el computes this from `user-emacs-directory' when it loads, which
;; happens right after this file. Set it explicitly so it can't drift.
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Redirect eln-cache into var/
(when (and (featurep 'native-compile)
           (fboundp 'startup-redirect-eln-cache))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))
