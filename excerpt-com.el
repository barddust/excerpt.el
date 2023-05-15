
;;; require
(require 'emacsql)
(require 'emacsql-sqlite)

(defcustom excerpt-dir (f-expand "excerpt/"
                                 user-emacs-directory)
  "Where to save excerpt.el stuffs")

(defcustom excerpt-database-file nil
  "Database")

(defvar excerpt-db nil
  "Handle of sqlite db")

(defun excerpt-db-initialize ()
  "Create database and tables."
  (unless (f-dir-p excerpt-dir)
    (make-directory excerpt-dir))
  
  (setq excerpt-database-file (f-expand "excerpt.db" excerpt-dir)
        excerpt-db (emacsql-sqlite excerpt-database-file))
  (emacsql excerpt-db [:create-table
                       :if-not-exists excerpt
                       ([(id integer :primary-key) content date tags])])
  (emacsql excerpt-db [:create-table
                       :if-not-exists review
                       ([key value])]))

(defun excerpt--get-excerpt (id)
  "Return data of the excerpt with ID."
  (car
   (emacsql excerpt-db [:select [id content date tags]
                        :from excerpt
                        :where (= id $s1)]
            id)))

(defun excerpt--add-excerpt (content date tags)
  "Add new excerpt."
  (emacsql excerpt-db [:insert-into excerpt
                       :values $v1]
           (list (vector nil content date tags))))

(defun excerpt--set-excerpt (id content date tags)
  "Set the excerpt."
  (emacsql excerpt-db [:update excerpt
                       :set [(= content $s2)
                             (= date $s3)
                             (= tags $s4)]
                       :where (= id $s1)]
           id content date tags))

(defun excerpt--delete-excerpt (id)
  "Delete the excerpt."
  (emacsql excerpt-db [:delete
                       :from excerpt
                       :where (= id $s1)]
           id))

(defun excerpt--search-by-tags (tags)
  (reverse
   (emacsql excerpt-db
            (vector
             :select '*
             :from 'excerpt
             :where (cons 'or (--map
                               (backquote
                                (like tags
                                      (quote
                                       ,(format "%%%s%%" it))))
                               tags))
             :order-by 'date))))

(defun excerpt--search-in-date (date)
  (reverse
   (emacsql excerpt-db [:select *
                        :from excerpt
                        :where (= date $s1)
                        :order-by date]
            date)))

(defun excerpt--search-by-content (strings)
  "Search all the excerpts containing strings,
in which each are seperated by `SPACEL'"
  (reverse
   (emacsql excerpt-db
           (vector
            :select '*
            :from 'excerpt
            :where
            (if (length> strings 0)
                (cons 'or (--map
                           (backquote
                            (like content
                                  (quote
                                   ,(format "%%%s%%" it))))
                           (s-split " "
                                    strings
                                    t)))
              'true)
            :order-by 'date))))

(defun excerpt--get-tags-list ()
  "Get all the tags, return a list."
  (--reduce-from
   (-union (car it) acc)
   nil
   (emacsql excerpt-db [:select tags
                        :from excerpt])))

(defun excerpt--get-date-list ()
  "Return a list containing all the date at which there are excerpts."
  (--reduce
   (-union acc it)
   (emacsql excerpt-db [:select date
                        :from excerpt
                        :order-by date])))

(defun excerpt--load (data)
  "DATA: {data: [{content: \"cnt\" , date: \"1970-01-01\", tags: []}]}"
  (let ((lst (--map
              (vector nil
                      (gethash "content" it)
                      (gethash "date" it)
                      (append (gethash "tags" it) nil))
              (append (gethash "data" data) nil))))
    (emacsql excerpt-db [:insert-into excerpt
                         :values $v1]
             lst)
    (message "Excerpts loaded: %d Total." (length lst))))

(defun excerpt--dump (data)
  "DATA: ((content date (tags)) ...)"
  (let ((vec (apply #'vector
                    (--map
                     (seq-let (id content date tags) it
                       `((content . ,content)
                         (date . ,date)
                         (tags . ,(apply #'vector tags))))
                     data))))
    (message "Excerpts dumps: %d Total." (length vec))
    (json-serialize (list (cons 'data vec)))))

(provide 'excerpt-com)
