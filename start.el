(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun is-installed (str)
  (eq 0
      (car (process-exit-code-and-output "bash" "-c" (concat "command -v " str)))))

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

(defun string-join (strings delim)
  (mapconcat 'identity strings delim))

(defun print-section (str)
  (let ((to-add-left (/ (- 80 (length str)) 2.0))
        (to-add-right))

    (setq to-add-right (if (integerp to-add-left) to-add-left (ceiling to-add-left)))
    (message (concat (make-string (floor to-add-left) ?-) (concat " " str " ") (make-string to-add-right ?-)))))


(let ((emacs-in-config-dir (expand-file-name "~/.config/.emacs.d"))
      (emacs-dir (expand-file-name "~/.emacs.d")))
  (cond
   ((file-directory-p emacs-dir)           (clone-in-folder-if-necessary emacs-dir))
   ((file-directory-p emacs-in-config-dir) (clone-in-folder-if-necessary emacs-in-config-dir))))

(print-section "checking apt progs")

(let ((to-check '("vim" "fzf"))
      (to-install '())
      (cmd))

  (dolist (p to-check)
    (if (is-installed p)
        (message "%s is installed" p)
      (progn
	      (message "installing %s" p)
        (push p to-install))))

  (when to-install
    (setq cmd (concat "apt install " (string-join to-install " ") " -y"))
    (message "running %s" cmd)
    (message "%s" (sudo-shell-command cmd))))

(print-section "checking snap progs")
(let ((to-check '(("discord" . "")
                  ("spotify" . "")))
      (to-install nil))

  (dolist (p to-check)
    (let ((name (car p)))
      (if (is-installed name)
          (message "%s is installed" name)
        (progn
          (push p to-install)))))

  (dolist(app to-install)
    (message "Installing %s" (car app))
    (message "%s" (sudo-shell-command
                   (concat "snap install "
                           (car app)
                           " "
                           (cdr app))))))

(print-section "Checking symlinks")

(defvar-local working-dir (file-name-directory load-file-name))
(defvar-local symlinks-to-check '((".spacemacs" . "~/.spacemacs")
                                  (".tmux.conf" . "~/.tmux.conf")
                                  (".zshrc" . "~/.zshrc")
                                  ("usq" . "~/.config/usq")))
(mapcar (lambda (el)
          (let ((file-here (car el))
                (symlink-location (cdr el)))
            (princ (format "checking %s..." symlink-location))
            (if (file-symlink-p symlink-location)
                (princ "symlink ok\n")
              (progn
                (print (format "create %s symlink" symlink-location))
                (make-symbolic-link (concat working-dir file-here) symlink-location)))))
        symlinks-to-check)

(print-section "ZSH setup")
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
(print-section "git config")
(with-temp-buffer
  (let ((b (current-buffer)))
    (shell-command "git config --global pull.rebase true" b)
    (shell-command "git config --global user.name Michael Conrads" b)
    (shell-command "git config --global user.email michaelconrads@me.com" b)
    (shell-command "git config --global alias.st status" b)
    (shell-command "git config --global alias.cm commit" b)
    (shell-command "git config --global alias.br branch" b)))
(message "done")


;; TODO:
;; + org-brain setup
;; + keyboard speed
;; + init ?








