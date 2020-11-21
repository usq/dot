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

(defun git-dir-p (dir)
  (and (file-directory-p (expand-file-name dir))
       (file-directory-p (concat (file-name-as-directory dir) ".git"))))


(defun clone-in-folder-if-necessary (dir)
  (message "checking folder %s for emacs" dir)
  (if (git-dir-p dir)  ;; emacs-dir exists
      (message "Doing nothing with dir %s, already a git dir" dir)
    (progn
      (delete-directory dir)
      (message "cloning spacemacs...")
      (print-shell-command "git" "clone" "https://github.com/syl20bnr/spacemacs" dir "-b" "develop"))))

(let ((emacs-in-config-dir (expand-file-name "~/.config/.emacs.d"))
      (emacs-dir (expand-file-name "~/.emacs.d")))
  (cond
   ((file-directory-p emacs-dir)           (clone-in-folder-if-necessary emacs-dir))
   ((file-directory-p emacs-in-config-dir) (clone-in-folder-if-necessary emacs-in-config-dir))))


(defun sudo-shell-command (command)
  (shell-command-to-string (concat "echo " (read-passwd "Password: ") " | sudo -S " command)))

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
                                  ))
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
















