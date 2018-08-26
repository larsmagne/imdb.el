;;; imdb-mode.el --- querying the imdb movie database
;; Copyright (C) 2014 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; This file is not part of GNU Emacs.

;; imdb.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; imdb.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'imdb)
(require 'sqlite3)

(defvar imdb-data-people (make-hash-table :test #'equal))
(defvar imdb-data-films (make-hash-table :test #'equal))
(defvar imdb-data-participants (make-hash-table :test #'equal))
(defvar imdb-data-participated-in (make-hash-table :test #'equal))

(defvar imdb-db nil)

(defvar imdb-tables
  '((person
     (pid text :primary)
     (primary-name text)
     (birth-year integer)
     (death-year integer))
    (movie
     (mid text :primary)
     (type text)
     (primary-title text)
     (original-title text)
     (adultp text)
     (start-year integer)
     (end-year integer)
     (length integer))
    (person-primary-profession
     (pid text :references person)
     (profession text))
    (person-known-for
     (pid text :references person)
     (mid text :references movie))
    (movie-genre
     (mid text :references movie)
     (genre text))
    ))

(defun imdb-dehyphenate (elem)
  (replace-regexp-in-string "-" "_" (symbol-name elem)))

(defun imdb-create-tables ()
  (unless imdb-db
    (setq imdb-db (sqlite3-new "~/.emacs.d/imdb/imdb.sqlite3")))
  (loop for (table . columns) in imdb-tables
	do (sqlite3-execute-batch
	    imdb-db (format
		     "create table if not exists %s (%s)"
		     (imdb-dehyphenate table)
		     (mapconcat
		      #'identity
		      (loop for elem in columns
			    collect (format
				     "%s %s%s%s"
				     (imdb-dehyphenate (car elem))
				     (cadr elem)
				     (if (memq :primary elem)
					 " primary key"
				       "")
				     (let ((references
					    (cadr (memq :references elem))))
				       (if references
					   (format " references %s"
						   references)
					 ""))))
		      ", ")))))

(defun imdb-read-line ()
  (loop for elem in (split-string
		     (buffer-substring (point) (line-end-position))
		     "\t")
	collect (if (equal elem "\\N")
		    nil
		  elem)))

(defun imdb-read-data ()
  (with-temp-buffer
    (sqlite3-execute-batch imdb-db "delete from movie")
    (insert-file-contents "~/.emacs.d/imdb/title.basics.tsv")
    (forward-line 1)
    (while (not (eobp))
      (let* ((elem (imdb-read-line))
	     (object (imdb-make 'movie elem)))
	(imdb-insert object)
	(loop for genre in (split-string (car (last elem)) ",")
	      do (imdb-insert (imdb-make 'movie-genre
					 (list (car elem) genre)))))
      (forward-line 1))))

(defun imdb-make (table values)
  (nconc (list :type table)
	 (loop for column in (cdr (assq table imdb-tables))
	       for value in values
	       for type = (cadr column)
	       append (list (intern (format ":%s" (car column)) obarray)
			    (cond
			     ((and value
				   (or (eq type 'integer)
				       (eq type 'number)
				       (eq type 'real)))
			      (string-to-number value))
			     (t
			      value))))))

(defun imdb-exec (statement values)
  ;;(message "%s %S" statement values)
  (sqlite3-execute-batch imdb-db statement values))

(defun imdb-column-name (column)
  (replace-regexp-in-string ":" "" (imdb-dehyphenate column)))

(defun imdb-insert (object)
  (imdb-exec
   (format "insert into %s(%s) values(%s)"
	   (imdb-dehyphenate (getf object :type))
	   (mapconcat
	    #'identity
	    (loop for (column nil) on (cddr object) by #'cddr
		  collect (imdb-column-name column))
	    ",")
	   (mapconcat
	    #'identity
	    (loop repeat (/ (length (cddr object)) 2)
		  collect "?")
	    ","))
   (coerce
    (loop for (nil value) on (cddr object) by #'cddr
	  collect value)
    'vector)))

(defvar imdb-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "f" 'imdb-mode-search-film)
    (define-key map "p" 'imdb-mode-search-person)
    (define-key map "x" 'imdb-mode-toggle-insignificant)
    (define-key map "\r" 'imdb-mode-select)
    map))

(define-derived-mode imdb-mode special-mode "Imdb"
  "Major mode for examining the imdb database.

\\{imdb-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq-local imdb-mode-filter-insignificant nil)
  (setq-local imdb-mode-mode 'film-search)
  (setq-local imdb-mode-search nil)
  (setq truncate-lines t))

(defun imdb-search (film)
  "Create a buffer to examine the imdb database."
  (interactive "sFilm: ")
  (switch-to-buffer "*imdb*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (imdb-mode)
    (imdb-mode-search-film film)))

(defun imdb-mode-toggle-insignificant ()
  "Toggle whether to list proper films only."
  (interactive)
  (setq imdb-mode-filter-insignificant
	(not imdb-mode-filter-insignificant))
  (message "Unimportant items are now %s"
	   (if imdb-mode-filter-insignificant
	       "filtered"
	     "not filtered"))
  (imdb-mode-refresh-buffer))

(defun imdb-mode-refresh-buffer ()
  (let ((ids (save-excursion
	       (loop repeat 20
		     while (not (eobp))
		     collect (get-text-property (point) 'id)
		     do (forward-line 1)))))
  (cond
   ((eq imdb-mode-mode 'film-search)
    (imdb-mode-search-film imdb-mode-search))
   ((eq imdb-mode-mode 'person)
    (imdb-mode-display-person imdb-mode-search)))
  (if (not ids)
      (goto-char (point-max))
    (loop for id in ids
	  while (not (imdb-mode-goto-id id))))))

(defun imdb-mode-goto-id (id)
  (let ((start (point))
	match)
    (goto-char (point-min))
    (if (setq match (text-property-search-forward 'id id t))
	(progn
	  (goto-char (prop-match-beginning match))
	  (beginning-of-line)
	  t)
      (goto-char start)
      nil)))

(defun imdb-mode-search-film (film)
  "List films matching FILM."
  (interactive "sFilm: ")
  (let ((films nil)
	(inhibit-read-only t))
    (setq imdb-mode-mode 'film-search
	  imdb-mode-search film)
    (erase-buffer)
    (maphash
     (lambda (key value)
       (when (string-match film (car value))
	 (push (cons key value) films)))
     imdb-data-films)
    (setq films (imdb-mode-filter films))
    (unless films
      (error "No films match %S" film))
    (dolist (film (cl-sort films 'string<
			   :key (lambda (e)
				  (nth 2 e))))
      (insert
       (propertize
	(format "%s %s%s%s\n"
		(propertize (nth 2 film) 'face 'variable-pitch)
		(propertize " " 'display '(space :align-to 8))
		(propertize (nth 1 film) 'face 'variable-pitch)
		(if (equal (nth 3 film) "movie")
		    ""
		  (propertize (format " (%s)" (nth 3 film))
			      'face '(variable-pitch
				      (:foreground "#80a080")))))
	'id (car film))))
    (goto-char (point-min))))

(defun imdb-mode-search-person (person)
  "List films matching PERSON."
  (interactive "sPerson: ")
  (let ((films nil)
	(inhibit-read-only t))
    (setq imdb-mode-mode 'people-search
	  imdb-mode-search person)
    (erase-buffer)
    (maphash
     (lambda (key value)
       (when (string-match person (car value))
	 (dolist (film (gethash key imdb-data-participated-in))
	   (push (append (cons (car film) (gethash (car film) imdb-data-films))
			 value)
		 films))))
     imdb-data-people)
    (setq films (imdb-mode-filter films))
    (unless films
      (error "No films match %S" person))
    (dolist (film (cl-sort films 'string<
			   :key (lambda (e)
				  (nth 2 e))))
      (insert
       (propertize
	(format "%s %s%s%s %s\n"
		(propertize (nth 2 film) 'face 'variable-pitch)
		(propertize " " 'display '(space :align-to 8))
		(propertize (nth 1 film) 'face 'variable-pitch)
		(if (equal (nth 3 film) "movie")
		    ""
		  (propertize (format " (%s)" (nth 3 film))
			      'face '(variable-pitch
				      (:foreground "#80a080"))))
		(nth 4 film))
	'id (car film))))
    (goto-char (point-min))))

(defun imdb-mode-filter (films)
  (if (not imdb-mode-filter-insignificant)
      films
    (loop for film in films
	  when (and (not (equal (nth 2 film) "?"))
		    (equal (nth 3 film) "movie"))
	  collect film)))

(defun imdb-mode-select ()
  "Select the item under point and display details."
  (interactive)
  (let ((id (get-text-property (point) 'id)))
    (cond
     ((or (eq imdb-mode-mode 'film-search)
	  (eq imdb-mode-mode 'people-search)
	  (eq imdb-mode-mode 'person))
      (imdb-mode-display-film id))
     (t
      (switch-to-buffer (format "*imdb %s*"
				(car (gethash id imdb-data-people))))
      (let ((inhibit-read-only t))
	(imdb-mode)
	(imdb-mode-display-person id))))))

(defun imdb-mode-display-film (id)
  (switch-to-buffer (format "*imdb %s*"
			    (car (gethash id imdb-data-films))))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (imdb-mode)
    (setq imdb-mode-mode 'film)
    (dolist (participant (cl-sort
			  (reverse (gethash id imdb-data-participants)) '<
			  :key (lambda (e)
				 (let ((job (nth 1 e)))
				   (cond
				    ((equal job "director") 1)
				    ((equal job "actor") 2)
				    ((equal job "actress") 2)
				    ((equal job "writer") 4)
				    (t 5))))))
      (insert (propertize
	       (format
		"%s %s%s\n"
		(propertize
		 (car (gethash (car participant) imdb-data-people))
		 'face 'variable-pitch)
		(propertize
		 (format "(%s)" (nth 1 participant))
		 'face '(variable-pitch (:foreground "#c0c0c0")))
		(if (equal (nth 2 participant) "\\N")
		    ""
		  (propertize
		   (format " %s" (nth 2 participant))
		   'face '(variable-pitch (:foreground "#808080")))))
	       'id (car participant))))
    (goto-char (point-min))))

(defun imdb-mode-display-person (id)
  (let ((inhibit-read-only t)
	films)
    (erase-buffer)
    (setq imdb-mode-mode 'person
	  imdb-mode-search id)
    (dolist (film (reverse (gethash id imdb-data-participated-in)))
      (push (append (list (car film))
		    (gethash (car film) imdb-data-films)
		    film)
	    films))
    (setq films (cl-sort (nreverse films) 'string<
			 :key (lambda (e)
				(nth 2 e))))
    (setq films (imdb-mode-filter films))
    (dolist (film films)
      (unless (equal (nth 3 film) "tvEpisode") 
	(insert (propertize
		 (format "%s %s%s%s%s%s%s\n"
			 (propertize (nth 2 film) 'face 'variable-pitch)
			 (propertize " " 'display '(space :align-to 8))
			 (propertize (nth 1 film) 'face 'variable-pitch)
			 (if (equal (nth 3 film) "movie")
			     ""
			   (propertize (format " (%s)" (nth 3 film))
				       'face '(variable-pitch
					       (:foreground "#a0a0a0"))))
			 (propertize (format " (%s)" (nth 5 film))
				     'face '(variable-pitch
					     (:foreground "#c0c0c0")))
			 (if (equal (nth 6 film) "\\N")
			     ""
			   (propertize (format " %s" (nth 6 film))
				       'face '(variable-pitch
					       (:foreground "#808080"))))
			 (let ((director
				(loop for (pid job text) in
				      (gethash (car film) imdb-data-participants)
				      when (equal job "director")
				      return (car (gethash pid imdb-data-people)))))
			   (if (not director)
			       ""
			     (propertize (format " %s" director)
					 'face '(variable-pitch
						 (:foreground "#80a080"))))))
		 'id (car film)))))
    (goto-char (point-min))))

(provide 'imdb-mode)

;;; imdb-mode.el ends here
