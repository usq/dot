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







(file-exists-p "~/.spacemacs")

(file-symlink-p "~/.spacemacs")






