(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun is-installed (str)
  (eq 0
      (car (process-exit-code-and-output "bash" "-c" "command" "-v" str))))


(if (is-installed "git")
    (message "git is installed")
  (message "not installed"))

(message "----------------------")
(message "Checking symlinks...")

(defvar-local working-dir (file-name-directory load-file-name))
(defvar-local symlinks-to-check '((".spacemacs" . "~/.spacemacs")
                                 (".tmux.conf" . "~/.tmux.conf")))
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
















