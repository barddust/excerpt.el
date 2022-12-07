(require 'excerpt-edit)
(require 'excerpt-buffer)
(require 'excerpt-filter)
(require 'excerpt-test)

(defun excerpt-next-tag (&optional N)
  "Move point to next tag.

With numeric prefix N, then to later N tag."
  (interactive "p")
  (dotimes (i (or N 1))
    (excerpt-buffer--next-tag)))

(defun excerpt-previous-tag (&optional N)
  "Move point to previous tag.

With numeric prefix N, then to former N tag."
  (interactive "p")
  (dotimes (i (or N 1))
    (excerpt-buffer--previous-tag)))

(defun excerpt-next-excerpt (&optional N)
  "Move point to next excerpt.

With numeric prefix N, then to later N excerpt."
  (interactive "p")
  (dotimes (i (or N 1))
    (excerpt-buffer--next-excerpt)))

(defun excerpt-previous-excerpt (&optional N)
  "Move point to previous excerpt.

With numeric prefix N, then to former N excerpt."
  (interactive "p")
  (dotimes (i (or N 1))
    (excerpt-buffer--previous-excerpt)))

(defun excerpt-add-excerpt ()
  "Add a new excerpt."
  (interactive)
  (excerpt-edit--add-excerpt))

(defun excerpt-edit-excerpt ()
  "Edit the excerpt at point."
  (interactive)
  (excerpt-edit--modify-excerpt))

(defun excerpt-delete-excerpt ()
  "Delete the excerpt at point."
  (interactive)
  (when (yes-or-no-p "Confirm to delete: ")
    (excerpt-edit--delete-excerpt)))

(defun excerpt-refresh ()
  "Refresh excerpt buffer"
  (interactive)
  (excerpt-buffer-build))

(defun excerpt-import (filename)
  "Import excerpts from JSON."
  (interactive "fImport from: ")
  (let ((js nil))
    (with-temp-buffer
      (find-file filename)
      (beginning-of-buffer)
      (setq js (json-parse-buffer))
      (kill-buffer))
    (excerpt--load js)))

(defun excerpt-export (filename)
  "Export excerpts in current buffer to JSON."
  (interactive "FExport to: ")
  (with-temp-file filename
    (erase-buffer)
    (insert (excerpt--dump
             (excerpt-buffer--get-excerpts)))))

(defvar excerpt-database-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'excerpt-import)
    (define-key map (kbd "e") 'excerpt-export)
    map))

(defvar excerpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'excerpt-next-excerpt)
    (define-key map (kbd "k") 'excerpt-previous-excerpt)
    (define-key map (kbd "n") 'excerpt-next-excerpt)
    (define-key map (kbd "p") 'excerpt-previous-excerpt)
    (define-key map (kbd "l") 'excerpt-next-tag)
    (define-key map (kbd "h") 'excerpt-previous-tag)
    (define-key map (kbd "<tab>") 'excerpt-next-tag)
    (define-key map (kbd "<backtab>") 'excerpt-previous-tag)
    (define-key map (kbd "a") 'excerpt-add-excerpt)
    (define-key map (kbd "e") 'excerpt-edit-excerpt)
    (define-key map (kbd "d") 'excerpt-delete-excerpt)
    (define-key map (kbd "g") 'excerpt-refresh)
    (define-key map (kbd "f") excerpt-filter-map)
    (define-key map (kbd "x") excerpt-database-map)
    map))

(define-derived-mode excerpt-mode
  special-mode "Excerpt"
  "Major mode of excerpts management."
  )

;;;###autoload
(defun excerpt ()
  (interactive)
  (unless excerpt-buffer
    (excerpt-db-initialize)
    (unless (buffer-live-p excerpt-buffer)
      (setq excerpt-buffer (generate-new-buffer "*Excerpt*")
            excerpt-buffer-tags nil
            excerpt-buffer-date nil
            excerpt-buffer-content "")
      (with-current-buffer excerpt-buffer
        (excerpt-buffer-build)
        (excerpt-mode))))
  (switch-to-buffer excerpt-buffer))

(provide 'excerpt)
