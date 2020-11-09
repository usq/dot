(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(use-package piper
  :config
  (spacemacs/declare-prefix "o |" "piper")
  (spacemacs/set-leader-keys
    "|"     '("piper-ui"        . piper-user-interface)
    "o | |" '("piper-locally"   . piper)
    "o | d" '("other-directory" . piper-other)
    "o | r" '("piper-remotely"  . piper-remote)))

(defun mc::search-cpp-ref (search-term)
  (interactive "sSearch Term: ")
  (require 'url-util)
  (browse-url (string-join (list "https://en.cppreference.com/mwiki/index.php?title=Special:Search&search=" (url-hexify-string search-term)))))

;; custom bindings for treemacs
              (with-eval-after-load 'treemacs
                (define-key treemacs-mode-map (kbd "o a -") 'treemacs-visit-node-ace-vertical-split)
                (define-key treemacs-mode-map (kbd "o a /") 'treemacs-visit-node-ace-horizontal-split)
                (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))





              (setq doom-modeline-vcs-max-length 100
                    doom-modeline-buffer-encoding nil)

              ;; no bell
              (setq ring-bell-function 'ignore)
              (with-eval-after-load 'evil
                (defalias #'forward-evil-word #'forward-evil-symbol)
                ;; make evil-search-word look for symbol rather than word boundaries
                (setq-default evil-symbol-word-search t))

              (defun mc::toggle-notes ()
                (interactive)
                (let* ((all-prefs (window-prev-buffers))
                       (nextpr (car all-prefs))
                       (_buffer-name (buffer-name nil)))
                  (if (string-equal _buffer-name "notebook.org")
                      (switch-to-buffer (car nextpr))
                    (mc::open-notebook))))


              (defun mc::open-inbox () (interactive) (find-file "~/Dropbox/org/inbox.org"))
              (defun mc::open-notebook () (interactive) (find-file "~/dev/org/notebook.org"))
              (defun mc::open-reference () (interactive) (find-file "~/Dropbox/org/store.org"))

              (spacemacs/declare-prefix "o" "custom")
              (spacemacs/set-leader-keys
                "oi" 'mc::open-inbox
                "oo" 'mc::toggle-notes
                "or" 'mc::open-reference)

              (spacemacs/set-leader-keys
                "aa" 'org-agenda-list)

              (spacemacs/set-leader-keys
                "gh" 'git-gutter+-show-hunk-inline-at-point)
              (spacemacs/set-leader-keys-for-major-mode 'org-mode "tu" 'org-dblock-update)
              (spacemacs/set-leader-keys "qq" 'spacemacs/frame-killer)


              (setq helm-buffer-max-length nil)

              ;; u was set to downcase which sucks
              (unbind-key (kbd "u") evil-visual-state-map)
              (bind-key (kbd "u") 'undo-tree-undo evil-visual-state-map)

              (defun make-window-fringes-smaller-for-git-gutter ()
                (if (bound-and-true-p git-gutter+-mode)
                    (set-window-fringes nil 4 8)
                  (set-window-fringes nil nil)))
              (add-hook 'git-gutter+-mode-hook 'make-window-fringes-smaller-for-git-gutter)


              ;; better grep:
              (setq grep-files-aliases
                    '(("all" . "* .[!.]* ..?*")
                      ("el" . "*.el")
                      ("ch" . "*.[ch]")
                      ("c" . "*.c")
                      ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
                      ("cch" . "*.h *.cc *.cpp")
                      ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
                      ("h" . "*.h")
                      ("m" . "[Mm]akefile*")
                      ("tex" . "*.tex")))

(setq vc-follow-symlinks t)
(setq undo-tree-enable-undo-in-region t)
(helm-icons-enable)
(setq projectile-enable-caching t
      c-basic-offset 4)

(defadvice compile (around use-bashrc activate)
  "Load .bashrc in any calls to bash (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    ad-do-it))
(setq compilation-scroll-output t) ;; make compilation window scroll
;;(setq compilation-scroll-output 'first-error) ;; stop at first error

(setq scroll-conservatively 101
      scroll-margin 6
      scroll-preserve-screen-position 't)

(defadvice magit-diff-visit-file-other-window (after fix-git-gutter activate) (make-window-fringes-smaller-for-git-gutter))
(with-eval-after-load 'magit
  (define-key magit-hunk-section-map (kbd "M-<return>") 'magit-diff-visit-file-other-window)
  (define-key magit-hunk-section-map (kbd "C-<return>") 'magit-diff-visit-file-other-window)
  (define-key magit-file-section-map (kbd "M-<return>") 'magit-diff-visit-file-other-window)
  (define-key magit-file-section-map (kbd "C-<return>") 'magit-diff-visit-file-other-window))

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq tramp-default-method "ssh")
(setq tramp-verbose 3)
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(setq dired-listing-switches "-alh --group-directories-first")

(setq org-confirm-babel-evaluate nil)

(use-package org-tempo :after org)

;;more overlay colors
(defface symbol-overlay-face-9
  '((t (:background "orange" :foreground "black")))
  "Symbol Overlay default candidate 9"
  :group 'symbol-overlay)

(defface symbol-overlay-face-10
  '((t (:background "medium purple" :foreground "black")))
  "Symbol Overlay default candidate 8"
  :group 'symbol-overlay)
(add-to-list 'symbol-overlay-faces 'symbol-overlay-face-9)
(add-to-list 'symbol-overlay-faces 'symbol-overlay-face-10)

(setq lsp-file-watch-ignored '(
                               "[/\\\\]\\.git$"
                               "[/\\\\]\\.hg$"
                               "[/\\\\]\\.bzr$"
                               "[/\\\\]_darcs$"
                               "[/\\\\]\\.svn$"
                               "[/\\\\]_FOSSIL_$"
                               "[/\\\\]\\.idea$"
                               "[/\\\\]\\.bitbucket$"
                               "[/\\\\]\\.ensime_cache$"
                               "[/\\\\]\\.clwb$"
                               "[/\\\\]\\.vscode$"
                               "[/\\\\]\\.eunit$"
                               "[/\\\\]node_modules$"
                               "[/\\\\]\\.fslckout$"
                               "[/\\\\]\\.tox$"
                               "[/\\\\]\\.stack-work$"
                               "[/\\\\]\\.bloop$"
                               "[/\\\\]\\.metals$"
                               "[/\\\\]target$"
                               "[/\\\\]\\.deps$"
                               "[/\\\\]build-aux$"
                               "[/\\\\]autom4te.cache$"
                               ".cache/bazel"
                               "[/\\\\]\\.ccls-cache$"
                               "bazel-out"
                               "bazel-werkstatt"
                               "bazel-bin"
                               "bazel-testlogs"
                               "[/\\\\]\\.reference$"
                               "[/\\\\]bazel-out$"
                               "[/\\\\]bazel-werkstatt$"
                               "[/\\\\]bazel-bin$"
                               "[/\\\\]bazel-testlogs$"
                               "/home/conradmi/.cache"
                               "bazel-genfiles$"
                               )
      lsp-idle-delay 0.5
      evil-want-Y-yank-to-eol nil
      writeroom-width 160
      )

(spacemacs/set-leader-keys "o c" 'mc::search-cpp-ref)
