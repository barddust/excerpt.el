
(require 'excerpt-com)

(defvar excerpt-review-id nil)

(defun excerpt-review-initialize ()
  (unless (emacsql excerpt-db [:select key
                               :from review])
    (emacsql excerpt-db [:insert-into review
                         :values (["reviewed" ()]
                                  ["review-id" "0000-00-00.0"])])))

(defun excerpt-review---get-reviewed-ids ()
  (-flatten
   (emacsql excerpt-db [:select value
                        :from review
                        :where (= key "reviewed")])))

(defun excerpt-review--save-reviewed-ids (ids)
  (emacsql excerpt-db [:update review
                       :set (= value $s1)
                       :where (= key "reviewed")]
           ids))

(defun excerpt-review--get-ids-exclude (ids)
  (-flatten
   (emacsql excerpt-db [:select id
                        :from excerpt
                        :where (not-in id $v1)]
            (apply #'vector ids))))

(defun excerpt-review--try-to-get-today ()
  (let ((res (caar
              (emacsql excerpt-db [:select value
                                   :from review
                                   :where (= key "review-id")]))))
    (when res
      (seq-let (date id) (s-split "\\." res t)
        (when (string= (format-time-string "%Y-%m-%d")
                       date)
          (string-to-number id))))))
    
(defun excerpt-review--update-today (id)
  (emacsql excerpt-db [:update review
                       :set (= value $s1)
                       :where (= key "review-id")]
           (format-time-string (format "%%Y-%%m-%%d.%d"
                                       id))))

(defun excerpt-review--generate-new-today-id ()
  (let* ((exclued-ids (excerpt-review---get-reviewed-ids))
         (ids (excerpt-review--get-ids-exclude exclued-ids))
         (id (nth (random (length ids)) ids)))
    
    
    (excerpt-review--save-reviewed-ids
     (if (length< ids 2)
      ;; refresh reviewed ids
         (list id)
       (cons id exclued-ids)))
    (excerpt-review--update-today id)
    id))

(defun excerpt-review--get-today-id ()
  (or excerpt-review-id
      (excerpt-review--try-to-get-today)
      (setq excerpt-review-id
            (excerpt-review--generate-new-today-id))))

(defun excerpt-review-get-excerpt ()
  "Try to initialize database, return review a EXCERPT."
  (unless excerpt-db
    (excerpt-db-initialize))
  (unless excerpt-review-id
    (excerpt-review-initialize))
  (excerpt--get-excerpt (excerpt-review--get-today-id)))

(provide 'excerpt-review)
