(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun is-installed (str)
  (eq 0
      (car (process-exit-code-and-output "bash" "-c" "command" "-v" str))))

(defun print-shell-command (&rest args)
  (let ((res (apply 'process-exit-code-and-output args)))
    (message (cadr res))
    (car res)))

(if (is-installed "git")
    (message "git is installed")
  (message "not installed"))

(defun mc:git-dir-p (dir)
  (and (file-directory-p (expand-file-name dir))
       (file-directory-p (concat (file-name-as-directory dir) ".git"))))

(defun mc:git-clone (url dir &optional branch)
  (let ((additional '()))
    (when branch
      (setq additional '("-b" branch)))
    (apply 'print-shell-command "git" "clone" url dir additional)))

(defun clone-in-folder-if-necessary (dir)
  (message "checking folder %s for emacs" dir)
  (if (mc:git-dir-p dir)  ;; emacs-dir exists
      (message "Doing nothing with dir %s, already a git dir" dir)
    (progn
      (delete-directory dir)
      (message "cloning spacemacs...")
      (mc:git-clone "https://github.com/syl20bnr/spacemacs" dir "develop"))))

(defun sudo-shell-command (command)
  (shell-command-to-string (concat "echo " (read-passwd "Password: ") " | sudo -S " command)))


(let ((emacs-in-config-dir (expand-file-name "~/.config/.emacs.d"))
      (emacs-dir (expand-file-name "~/.emacs.d")))
  (cond
   ((file-directory-p emacs-dir)           (clone-in-folder-if-necessary emacs-dir))
   ((file-directory-p emacs-in-config-dir) (clone-in-folder-if-necessary emacs-in-config-dir))))


(message "----------------------")
(if (is-installed "vim")
    (message "vim is installed")
    (progn
	(message "getting basic vim")
	(message "%s" (sudo-shell-command "apt install vim -y"))))

(message "----------------------")
(message "Checking symlinks...")

(defvar-local working-dir (file-name-directory load-file-name))
(defvar-local symlinks-to-check '((".spacemacs" . "~/.spacemacs")
                                  (".tmux.conf" . "~/.tmux.conf")
                                  (".zshrc" . "~/.zshrc")
                                  ("usq" . "~/.config/usq")))
(mapcar (lambda (el)
          (let ((file-here (car el))
                (symlink-location (cdr el)))
            (message "checking %s" symlink-location)
            (if (file-symlink-p symlink-location)
                (message "symlink ok")
              (progn
                (message "create %s symlink" symlink-location)
                (make-symbolic-link (concat working-dir file-here) symlink-location)))))
        symlinks-to-check)

(message "----------------------")

(message "---------- ZSH setup ------------")
(if (mc:git-dir-p "~/.oh-my-zsh")
  (let ;; .oh-my-zsh here, check if we have to install plugins
      ((zsh-autosug-dir (expand-file-name "~/.oh-my-zsh/custom/plugins/zsh-autosuggestions"))
       (zsh-syntax-dir (expand-file-name "~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting")))
    (unless (file-directory-p zsh-autosug-dir)
      (message "No zsh-autosuggestions found, cloning")
      (mc:git-clone "https://github.com/zsh-users/zsh-autosuggestions" zsh-autosug-dir))

    (unless (file-directory-p zsh-syntax-dir)
      (message "No zsh-syntax hightlighting found, cloning")
      (mc:git-clone "https://github.com/zsh-users/zsh-syntax-highlighting" zsh-syntax-dir))

    (message "done"))
  (message "please go to 'https://github.com/ohmyzsh/ohmyzsh' and run oh my zsh install from there"))

;; git setup
(message "git config")
(print-shell-command "git" "config" "--global" "pull.rebase" "true")
(print-shell-command "git" "config" "--global" "user.name" "Michael Conrads")
(print-shell-command "git" "config" "--global" "user.email" "michaelconrads@me.com")
(print-shell-command "git" "config" "--global" "alias.st" "status")
(print-shell-command "git" "config" "--global" "alias.cm" "commit")
(print-shell-command "git" "config" "--global" "alias.br" "branch")


;; TODO:
;; + org-brain setup
;; + keyboard speed
;; + init







