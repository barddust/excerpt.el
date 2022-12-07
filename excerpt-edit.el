(require 'excerpt-com)
(require 'excerpt-buffer)

(defvar excerpt-edit-buffer nil
  "Buffer to edit excerpt.")

(defvar excerpt-edit--id nil
  "now editing excerpt's id")

(defconst excerpt-edit--comment-pattern "^;[^\n]*")

(defconst excerpt-edit--tag-pattern "#[^#\n]*")

(defun excerpt-edit-buffer-p ()
  "If current buffer is excerpt edit buffer."
  (equal (current-buffer) excerpt-edit-buffer))

(defun excerpt-edit-build-buffer ()
  (erase-buffer)
  (insert ";; This buffer is used to edit excerpt.\n"
          ";; Comment line starts with prefix `;'.\n"
          ";; Press `C-c C-s' to save and quite, \n"
          ";; and `C-c C-k' to quit without saving\n\n"
          ";; Words between `#'s or `#' and the end of line represents TAG\n"
          ";; e.g. #tag1 #tag two #tag to the end\n"

          ";; Temporarily, `C-c C-i' to insert a tag."
          
          ";; of coz, each tag can occupies a line.\n\n"
          ))

(defun excerpt-edit-insert-tag ()
  (interactive)
  (insert "#"
          (completing-read ""
                           (excerpt--get-tags-list)
                           nil
                           t)
          "\n"))

(defun excerpt-edit-create-buffer ()
  "Create buffer to edit excerpt"
  (unless (buffer-live-p excerpt-edit-buffer)
    (setq excerpt-edit-buffer (generate-new-buffer "*Excerpt Edit*"))
    (with-current-buffer excerpt-edit-buffer
      (excerpt-edit-build-buffer)
      (excerpt-edit-mode)))
  
  (switch-to-buffer-other-window excerpt-edit-buffer))

(defun excerpt-edit--add-excerpt ()
  (setq excerpt-edit--id nil)
  (excerpt-edit-create-buffer))

(defun excerpt-edit--modify-excerpt ()
  (setq excerpt-edit--id (get-char-property (point) 'id))
  (if (not excerpt-edit--id)
      (message "There is no excerpt at point.")
    (excerpt-edit-create-buffer)
    (seq-let (id content date tags)
        (excerpt--get-excerpt excerpt-edit--id)
      (--map
       (insert (format "#%s\n" it))
       tags)
      (insert "\n" content "\n"))))

(defun excerpt-edit--delete-excerpt ()
  (setq excerpt-edit--id (get-char-property (point) 'id))
  (if (not excerpt-edit--id)
      (message "There is no excerpt at point.")
    (excerpt--delete-excerpt excerpt-edit--id)
    (excerpt-buffer-build)))

(defun excerpt-edit--quit (save)
  (if (not (excerpt-edit-buffer-p))
      (message "Current buffer is not excerpt edit buffer.")
    (when save
      (if excerpt-edit--id
          (apply #'excerpt--set-excerpt
                 excerpt-edit--id
                 (excerpt-edit--get-buffer-data))
        (apply #'excerpt--add-excerpt
               (excerpt-edit--get-buffer-data))))
    (delete-window)
    (kill-buffer excerpt-edit-buffer)))


(defun excerpt-edit-quit-with-saving ()
  "Save the excerpt and quit the window."
  (interactive)
  (excerpt-edit--quit t)
  (excerpt-buffer-build)
  (unless (equal (get-text-property (point) 'id)
                 excerpt-edit--id)
    (beginning-of-buffer)
    (text-property-search-forward 'id excerpt-edit--id))
  (message "Excerpt's changes saved"))

(defun excerpt-edit-quit-without-saving ()
  "Throw the excerpt and quit the window."
  (interactive)
  (excerpt-edit--quit nil)
  (message "Excerpt's changes not saved"))

(defun excerpt-edit--get-buffer-data ()
  (let* ((string (s-trim (s-replace-regexp
                          excerpt-edit--comment-pattern ""
                          (buffer-substring-no-properties
                           (point-min) (point-max)))))
         
         (date (format-time-string "%Y-%m-%d"))
         (tags (--map (s-trim (substring (car it) 1))
                      (s-match-strings-all "#[^#\n]*" string)))
         (content (s-trim
                   (s-replace-regexp excerpt-edit--tag-pattern ""
                                     string))))
    (list content date tags)))

(defvar excerpt-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'excerpt-edit-quit-with-saving)
    (define-key map (kbd "C-c C-k") 'excerpt-edit-quit-without-saving)

    (define-key map (kbd "C-c C-i") 'excerpt-edit-insert-tag)
    map))

(defvar excerpt-edit-highlights
  `((,excerpt-edit--comment-pattern . 'font-lock-comment-face)
    (,excerpt-edit--tag-pattern . 'font-lock-keyword-face)))

(define-derived-mode excerpt-edit-mode
  text-mode "Excerpt Edit"
  "Major mode of excerpts editing."
  (setq-local font-lock-defaults '(excerpt-edit-highlights)))

(provide 'excerpt-edit)
