(require 'excerpt-com)
(require 'excerpt-buffer)

(defun excerpt-filter-by-content (&optional regex)
  "Filter by CONTENT. Using regular expression."
  (interactive)
  (when (interactive-p)
    (setq regex (read-from-minibuffer "Content: ")))
  (setq excerpt-buffer-tags nil
        excerpt-buffer-date nil
        excerpt-buffer-content regex)
  (excerpt-buffer-build))

(defun excerpt-filter-by-tags (&optional tags)
  "Filter by TAGS."
  (interactive)
  (setq excerpt-buffer-tags
        (if (interactive-p)
            (let ((candidates (excerpt--get-tags-list))
                  (prefix "Tags: "))
              (completing-read-multiple
               prefix
               candidates
               #'(lambda (para)
                   (let ((chosen
                          (s-split "\s*,\s*"
                                   (buffer-substring-no-properties
                                    (1+ (length prefix))
                                    (point-max))
                                   t))
                         (cand
                          (s-split "\s*,\s*"
                                   para
                                   t)))
                     (--remove (member it chosen)
                               cand)))
               t))
          (-map #'s-trim
                (s-split "\s*,\s*" tags t)))
        excerpt-buffer-date nil
        excerpt-buffer-content "")
  (excerpt-buffer-build))

(defun excerpt-filter-by-date (&optional date)
  "Filter by DATE."
  (interactive)
  (when (interactive-p)
    (let ((candidates (excerpt--get-date-list))
          (prefix "Date: "))
      (setq date
            (completing-read prefix
                             candidates
                             nil
                             t))))
  (setq excerpt-buffer-tags nil
        excerpt-buffer-date date
        excerpt-buffer-content "")
  (excerpt-buffer-build))

(defun excerpt-filter-all ()
  "Show all excerpts"
  (interactive)
  (setq excerpt-buffer-tags nil
        excerpt-buffer-date nil
        excerpt-buffer-content "")
  (excerpt-buffer-build))

(defvar excerpt-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'excerpt-filter-all)
    (define-key map (kbd "d") 'excerpt-filter-by-date)
    (define-key map (kbd "t") 'excerpt-filter-by-tags)
    (define-key map (kbd "s") 'excerpt-filter-by-content)
    map))

(provide 'excerpt-filter)
