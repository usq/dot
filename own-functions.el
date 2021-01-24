




(defun mc::search-cpp-ref (search-term)
  (interactive "sSearch Term: ")
  (require 'url-util)
  (browse-url (string-join (list "https://en.cppreference.com/mwiki/index.php?title=Special:Search&search=" (url-hexify-string search-term)))))


(defun mc::toggle-notes ()
  (interactive)
  (let* ((all-prefs (window-prev-buffers))
         (nextpr (car all-prefs))
         (_buffer-name (buffer-name nil)))
    (if (string-equal _buffer-name "notebook.org")
        (switch-to-buffer (car nextpr))
      (mc::open-notebook))))


(provide 'own-functions)

