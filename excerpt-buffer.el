
(require 'excerpt-com)

(defvar excerpt-buffer nil
  "Buffer to show excerpts")

(defvar excerpt-buffer-tags nil
  "Buffer showed by tags. Contradict with `excerpt-buffer-date'")

(defvar excerpt-buffer-date nil
  "Buffer showed by date. Contradict with `excerpt-buffer-tags'")

(defvar excerpt-buffer-content ""
  "Buffer showed by content.
It is used only when `excerpt-buffer-tags' and `excerpt-buffer-date'
are both nil")

(defun excerpt-buffer--next-tag ()
  (let ((at-hash (= (char-after) 35)))
    (condition-case nil
        (progn
          (when at-hash
            (forward-char))
          (re-search-forward "#[^\s]*" nil)
          (search-backward "#"))
    (error (progn
             (when at-hash
               (backward-char))
             (message "No more tags."))))))

(defun excerpt-buffer--previous-tag ()
  (condition-case nil
      (re-search-backward "#[^\s]*" nil)
    (error (message "No more tags."))))

(defun excerpt-buffer--next-excerpt ()
  (let ((at-beginning-of-line-and-hash
         (and (= (point)
                 (save-excursion
                   (beginning-of-line)
                   (point)))
              (= (char-after) 35))))
    (condition-case nil
        (progn
          (when at-beginning-of-line-and-hash
            (forward-char))
          (re-search-forward "^#.*" nil)
          (beginning-of-line))
    (error (progn
             (when at-beginning-of-line-and-hash
               (backward-char))
             (message "No more excerpts."))))))
  
(defun excerpt-buffer--previous-excerpt ()
  (let ((at-tag-line (save-excursion
                       (beginning-of-line)
                       (= (char-after) 35)))
        (at-point (point)))
  (condition-case nil
      (progn
        (when at-tag-line
          (beginning-of-line))
        (re-search-backward "^#.*" nil)
        (beginning-of-line))
    (error (progn
             (when at-tag-line
               (goto-char at-point))
           (message "No more excerpts."))))))

(defun excerpt-buffer--tag-button (button)
  "Callback for tag button."
  (setq excerpt-buffer-tags (list (substring (button-label button)
                                             1))
        excerpt-buffer-date nil)
  (excerpt-buffer-build))

(defun excerpt-buffer--date-button (button)
  "Callback for date button."
  (setq excerpt-buffer-tags nil
        excerpt-buffer-date (button-label button))
  (excerpt-buffer-build))

(defun excerpt-buffer--insert-excerpt (excerpt)
  "Insert a excerpt into buffer."
  (let ((stp (point)))
    (seq-let (id content date tags) excerpt
      (--map
       (progn
         (insert-button (format "#%s" it)
                        'action #'excerpt-buffer--tag-button
                        'face `(:foreground ,(face-attribute
                                              'font-lock-function-name-face
                                              :foreground)))
         (insert " "))
       tags)
      (insert "\n")

      (insert-button date
                     'action #'excerpt-buffer--date-button
                     'face `(:foreground ,(face-attribute
                                           'shadow
                                           :foreground)))
      
      (insert (format "\n%s\n" content))
      (add-text-properties stp (point)
                           `(id ,id))))
  (insert "\n\n"))

(defun excerpt-buffer--get-excerpts ()
  (cond
   (excerpt-buffer-tags (excerpt--search-by-tags excerpt-buffer-tags))
   (excerpt-buffer-date (excerpt--search-in-date excerpt-buffer-date))
   (t (excerpt--search-by-content excerpt-buffer-content))))

(defun excerpt-buffer--get-new-buffer-name ()
  (cond
   (excerpt-buffer-tags
    (format "*Excerpt%s*"
            (s-join " "
                    (--map
                     (format "#%s" it)
                     excerpt-buffer-tags))))
   
   (excerpt-buffer-date
    (format "*Excerpt@%s*" excerpt-buffer-date))
   
   (t (if (length> excerpt-buffer-content 0)
          (format "*Excerpt/%s*"
                  (if (length> excerpt-buffer-content 12)
                      (concat (substring excerpt-buffer-content
                                         0 12)
                              "...")
                    excerpt-buffer-content))
        "*Excerpt*"))))

(defun excerpt-buffer-build ()
  "Show excerpts to buffer, and rename buffer. "
  (read-only-mode -1)
  (erase-buffer)
  (-map #'excerpt-buffer--insert-excerpt
        (excerpt-buffer--get-excerpts))
  (rename-buffer (excerpt-buffer--get-new-buffer-name))
  (beginning-of-buffer)
  (read-only-mode 1))

(provide 'excerpt-buffer)
