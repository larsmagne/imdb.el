;;; imdb-mode.el --- querying the imdb movie database -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: movies

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

;; The Emacs has to be pretty new -- anything older than Emacs 20
;; probably won't work.  And it has to be built with module support.

;; After getting all that set up, you need to download the IMDB
;; dataset and create the sqlite3 database, which can be done with
;; this:

;; (imdb-download-and-create)

;; This will take several hours and use 10GB of disk space.  Emacs
;; will not be usable while this is going on.

;; Usage: `M-x imdb-person' or `M-x imdb-film'

;;; Code:

(eval-when-compile
  (setq byte-compile-warnings '(not cl-functions)))

(require 'imdb)
(require 'sqorm)
(require 'cl-lib)
(require 'browse-url)
(require 'open-web)

(defvar imdb-db nil)
(defvar imdb-db-directory (expand-file-name "imdb" user-emacs-directory))

(defvar imdb-mode-extra-data nil)
(defvar imdb-mode-filter-insignificant nil)
(defvar imdb-mode-filter-job nil)
(defvar imdb-mode-mode nil)
(defvar imdb-mode-search nil)

(defvar imdb-tables
  '((movie
     (mid text :primary)
     (type text)
     (primary-title text)
     (original-title text)
     (adultp bool)
     (start-year integer)
     (end-year integer)
     (length integer))
    (movie-genre
     (mid text :references movie)
     (genre text))
    
    (person
     (pid text :primary)
     (primary-name text)
     (birth-year integer)
     (death-year integer))
    (person-primary-profession
     (pid text :references person)
     (profession text))
    (person-known-for
     (pid text :references person)
     (mid text :references movie))

    (title
     (mid text :references movie)
     (rank integer)
     (title text)
     (region text)
     (language text)
     (types text)
     (attributes text)
     (originalp bool))

    (crew
     (mid text :references movie)
     (pid text :references person)
     (category text))
     
    (episode
     (mid text :primary :references movie)
     (movie text :references movie)
     (season integer)
     (episode integer))

    (rating
     (mid text :primary :references movie)
     (rating real)
     (votes integer))

    (principal
     (mid text :references movie)
     (rank integer)
     (pid text :references person)
     (category text)
     (job text))

    (principal-character
     (mid text :references movie)
     (pid text :references person)
     (character text))

    (person-search
     (primary-name text)
     (pid text))
    (movie-search
     (primary-title text)
     (mid text))))

(defun imdb-download-and-create ()
  "Download the IMDB data files and create the sqlite3 database.
This will take some hours and use 10GB of disk space."
  (imdb-download-data)
  (imdb-create-tables)
  (imdb-read-data)
  (imdb-create-indices)
  (imdb-populate-search))

(defun imdb-download-data ()
  (let ((dom
	 (with-current-buffer (url-retrieve-synchronously
			       "https://datasets.imdbws.com/")
	   (goto-char (point-min))
	   (search-forward "\n\n")
	   (prog1
	       (libxml-parse-html-region (point) (point-max))
	     (kill-buffer (current-buffer))))))
    (cl-loop for elem in (dom-by-tag dom 'a)
	     for url = (dom-attr elem 'href)
	     when (string-match "[.]gz\\'" url)
	     do (imdb-download-data-1 url))))

(defun imdb-download-data-1 (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (search-forward "\n\n")
    (zlib-decompress-region (point) (point-max))
    (let ((file (expand-file-name (replace-regexp-in-string
				   "[.]gz\\'" ""
				   (file-name-nondirectory
				    (url-filename
				     (url-generic-parse-url url))))
				  imdb-db-directory)))
      (unless (file-exists-p (file-name-directory file))
	(make-directory (file-name-directory file) t))
      (write-region (point) (point-max) file))
    (kill-buffer (current-buffer))))

(defun imdb-initialize ()
  (unless imdb-db
    (setq imdb-db (sqorm-open (expand-file-name "imdb.sqlite3"
                                                imdb-db-directory)))))

(defun imdb-create-tables ()
  (imdb-initialize)
  (sqorm-create-tables
   (cl-loop for (table . columns) in imdb-tables
	    unless (memq table '(person-search movie-search))
	    collect (cons table columns))))

(defun imdb-create-indices ()
  (imdb-create-index
   "pcidx on principal_character(mid, pid)"
   "mgidx on movie_genre(mid)"
   "pppidx on person_primary_profession(pid)"
   "pkfidx on person_known_for(pid)"
   "peidx on person(primary_name)"
   "tidx on title(mid)"
   "cidx on crew(mid, pid)"
   "eidx on episode(movie)"
   "pidx on principal(mid, pid)"
   "ppidx on principal(pid)"
   "mtidx on movie(primary_title)"))

(defun imdb-create-search-tables ()
  (sqlite-execute
   imdb-db
   "create virtual table person_search USING fts5 (primary_name, pid)"))

(defun imdb-create-index (&rest statements)
  (dolist (statement statements)
    (sqlite-execute
     imdb-db (concat "create index if not exists " statement))))

(defun imdb-read-line ()
  (cl-loop for elem in (split-string
			(buffer-substring (point) (line-end-position))
			"\t")
	   collect (if (equal elem "\\N")
		       nil
		     elem)))

(defun imdb-read-tsv (tables file function)
  (with-temp-buffer
    (dolist (table tables)
      (sqlite-execute imdb-db (format "delete from %s"
				      (sqorm-dehyphenate table))))
    (sqlite-transaction imdb-db)
    (insert-file-contents (format (expand-file-name "%s.tsv" imdb-db-directory)
                                  file))
    (forward-line 1)
    (let ((lines 1)
	  (total (count-lines (point-min) (point-max))))
      (while (not (eobp))
	(when (zerop (% (cl-incf lines) 10000))
	  (sqlite-commit imdb-db)
	  (sqlite-transaction imdb-db))
	(when (zerop (% (cl-incf lines) 1000))
	  (message "%s: Read %d lines (%.1f%%)" file lines
		   (* (/ (* lines 1.0) total) 100)))
	(funcall function (imdb-read-line))
	(forward-line 1)))
    (sqlite-commit imdb-db)))

(defun imdb-read-simple (table file)
  (imdb-read-tsv
   (list table) file
   (lambda (elem)
     (sqorm-insert (imdb-make table elem)))))

(defun imdb-read-data ()
  (imdb-read-tsv
   '(movie movie-genre) "title.basics"
   (lambda (elem)
     (sqorm-insert (imdb-make 'movie elem))
     (when (car (last elem))
       (cl-loop for genre in (split-string (car (last elem)) ",")
		do (sqorm-insert (imdb-make 'movie-genre
					    (list (car elem) genre)))))))

  (imdb-read-tsv
   '(person person-primary-profession person-known-for)
   "name.basics"
   (lambda (elem)
     (let ((person (imdb-make 'person elem)))
       (sqorm-insert person)
       (let ((professions (car (last elem 2)))
	     (known (car (last elem))))
	 (when professions
	   (cl-loop for profession in (split-string professions ",")
		    do (sqorm-insert (imdb-make
				      'person-primary-profession
				      (list (car elem) profession)))))
	 (when known
	   (cl-loop for k in (split-string known ",")
		    do (sqorm-insert (imdb-make 'person-known-for
						(list (car elem) k)))))))))

  (imdb-read-simple 'title "title.akas")

  (imdb-read-tsv
   '(crew) "title.crew"
   (lambda (elem)
     (let ((directors (cadr elem))
	   (writers (caddr elem)))
       (when directors
	 (dolist (mid (split-string directors ","))
	   (sqorm-insert (imdb-make 'crew (list (car elem) mid "director")))))
       (when writers
	 (dolist (mid (split-string writers ","))
	   (sqorm-insert (imdb-make 'crew (list (car elem) mid "writer"))))))))

  (imdb-read-simple 'episode "title.episode")
  (imdb-read-simple 'rating "title.ratings")

  (imdb-read-tsv
   '(principal principal-character) "title.principals"
   (lambda (elem)
     (let* ((object (imdb-make 'principal elem)))
       (sqorm-insert object)
       (when (car (last elem))
	 (with-temp-buffer
	   (insert (car (last elem)))
	   (goto-char (point-min))
	   (cl-loop for character across (json-read)
		    do (sqorm-insert (imdb-make 'principal-character
						(list (cl-getf object :mid)
						      (cl-getf object :pid)
						      character))))))))))

(defun imdb-make (table values)
  (sqorm-make table values imdb-tables))

(defvar imdb-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "f" 'imdb-film)
    (define-key map "p" 'imdb-person)
    (define-key map "x" 'imdb-mode-toggle-insignificant)
    (define-key map "l" 'imdb-mode-load-all-images)
    (define-key map "A" 'imdb-mode-person-age)
    (define-key map "a" 'imdb-mode-show-acting)
    (define-key map "d" 'imdb-mode-show-directing)
    (define-key map "&" 'imdb-mode-open-imdb)
    (define-key map "w" 'imdb-mode-copy-url)
    (define-key map "q" 'kill-current-buffer)
    (define-key map " " 'imdb-mode-mark-line)
    (define-key map "\r" 'imdb-mode-select)
    (define-key map "m" 'imdb-mode-display-intersection)
    map))

(define-derived-mode imdb-mode special-mode "Imdb"
  "Major mode for examining the imdb database.

\\{imdb-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq-local imdb-mode-filter-insignificant t)
  (setq-local imdb-mode-filter-job nil)
  (setq-local imdb-mode-mode 'film-search)
  (setq-local imdb-mode-search nil)
  (setq-local imdb-mode-extra-data nil)
  (setq truncate-lines t))

(defun imdb-search (film)
  "Create a buffer to examine the imdb database."
  (interactive "sFilm: ")
  (switch-to-buffer "*imdb*")
  (imdb-initialize)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (imdb-mode)
    (imdb-film film)))

(defun imdb-mode-mark-line ()
  "Toggle marking of the current line."
  (interactive)
  (let ((id (get-text-property (point) 'id))
	(inhibit-read-only t))
    (unless id
      (error "Nothing on the current line"))
    (let ((marked (get-text-property (line-beginning-position) 'mark)))
      (add-face-text-property
       (line-beginning-position) (line-end-position)
       (list :weight (if marked 'normal 'bold)))
      (put-text-property (line-beginning-position)
			 (1+ (line-beginning-position))
			 'mark (not marked)))))

(defun imdb-mode-person-age ()
  "Display the age of the actur under point when the movie was made."
  (interactive)
  (let (mid pid)
    (if (eq imdb-mode-mode 'person)
	(setq pid imdb-mode-search
	      mid (get-text-property (point) 'id))
      (setq pid (get-text-property (point) 'id)
	    mid imdb-mode-search))
    (let ((person (car (sqorm-select 'person :pid pid)))
	  (film (car (sqorm-select 'movie :mid mid))))
      (if (and (cl-getf film :start-year)
	       (cl-getf person :birth-year))
	  (message "%s was %s years old when %s was made"
		   (cl-getf person :primary-name)
		   (- (cl-getf film :start-year)
		      (cl-getf person :birth-year))
		   (cl-getf film :primary-title))
	(message "Insufficient data")))))

(defun imdb-mode-display-intersection ()
  "Show films that have all the people involved."
  (interactive)
  (let ((pids nil)
	match)
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'mark t t))
	(push (get-text-property (prop-match-beginning match) 'id) pids)))
    (unless pids
      (error "No marked people"))
    (switch-to-buffer (format "*imdb %s*"
			      (mapconcat
			       (lambda (pid)
				 (cl-getf (car (sqorm-select 'person :pid pid))
					  :primary-name))
			       pids
			       ",")))
    (let ((inhibit-read-only t))
      (imdb-mode)
      (imdb-mode-display-intersection-1
       pids
       (cl-loop for pid in pids
		collect (imdb-person-query-films pid))))))

(defun imdb-face (string &optional props)
  (setq string (if (stringp string)
		   string
		 (format "%s" string)))
  (propertize string
	      'face (cond
		     ((null props)
		      'variable-pitch)
		     ((stringp props)
		      `(variable-pitch (:foreground ,props)))
		     (t
		      `(variable-pitch ,props)))))		

(defun imdb-mode-display-intersection-1 (pids all)
  (let ((inhibit-read-only t)
	;; Find the intersection (i.e., films that all the people
	;; concerned have appeared in).
	(films
	 (cl-loop for film in (car all)
		  when (cl-every
			#'identity
			(cl-loop for other in (cdr all)
				 collect
				 (cl-member
				  film other
				  :test (lambda (e1 e2)
					  (equal (cl-getf e1 :mid)
						 (cl-getf e2 :mid))))))
		  collect film)))
    ;; Remove duplicates.
    (setq films
	  (cl-remove-duplicates films
				:key (lambda (e) (cl-getf e :mid))
				:test #'equal))
    (erase-buffer)
    (imdb-kill)
    (dolist (pid pids)
      (let ((start (point)))
	(imdb-insert-placeholder 300 400)
	(put-text-property start (point) 'id pid)
	(insert " ")))
    (imdb-load-people-images pids (current-buffer) 300 400 0)
    (insert "\n\n")
    (dolist (pid pids)
      (let ((person (car (sqorm-select 'person :pid pid))))
	(insert
	 (imdb-face (cl-getf person :primary-name) "#f0f0f0"))
	(when (cl-getf person :birth-year)
	  (insert
	   (imdb-face (format " (%s)"
			      (cl-getf person :birth-year))
		      "#a0a0a0"))))
      (insert " "))
    (insert "\n\n")
    (setq imdb-mode-mode 'intersection
	  imdb-mode-search (list pids all))
    (setq films (cl-sort
		 (reverse films) '<
		 :key (lambda (e)
			(or (cl-getf e :start-year) 1.0e+INF))))
    (setq films (imdb-mode-filter films))
    (dolist (film films)
      (imdb-mode-person-film film (car pids)))
    (goto-char (point-min))
    (forward-line 2)))

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

(defun imdb-mode-show-acting ()
  "Show only acting."
  (interactive)
  (setq imdb-mode-filter-job
	(if (eq imdb-mode-filter-job 'acting)
	    nil
	  'acting))
  (message
   (if imdb-mode-filter-job
       "Only showing acting"
     "Showing all jobs"))
  (imdb-mode-refresh-buffer))

(defun imdb-mode-show-directing ()
  "Show only directing."
  (interactive)
  (setq imdb-mode-filter-job
	(if (eq imdb-mode-filter-job 'directing)
	    nil
	  'directing))
  (message
   (if imdb-mode-filter-job
       "Only showing directing"
     "Showing all jobs"))
  (imdb-mode-refresh-buffer))

(defun imdb-mode-refresh-buffer ()
  (let ((ids (save-excursion
	       (cl-loop repeat 20
			while (not (eobp))
			collect (get-text-property (point) 'id)
			do (forward-line 1)))))
  (cond
   ((eq imdb-mode-mode 'film-search)
    (imdb-mode-search-film-1 imdb-mode-search))
   ((eq imdb-mode-mode 'intersection)
    (imdb-mode-display-intersection-1 (car imdb-mode-search)
				      (cadr imdb-mode-search)))
   ((eq imdb-mode-mode 'person)
    (imdb-mode-display-person imdb-mode-search)))
  (if (not ids)
      (goto-char (point-max))
    (cl-loop for id in ids
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

(defun imdb-film (film)
  "List films matching FILM."
  (interactive (list (imdb-complete-film)))
  (let ((mid (get-text-property 1 'id film)))
    (if mid
	(imdb-mode-show-film mid)
      (switch-to-buffer (format "*imdb %s*" film))
      (let ((inhibit-read-only t))
	(imdb-mode)
	(setq imdb-mode-mode 'film-search
	      imdb-mode-search film)
	(erase-buffer)
	(imdb-mode-search-film-1 film)))))

(defun imdb-mode-search-film-1 (film)
  (let ((films (if sqorm-regexp
		   (sqorm-select-where
		    "select * from movie where lower(primary_title) regexp ?"
		    film)
		 (sqorm-select-where
		  "select * from movie where lower(primary_title) like ?"
		  (format "%%%s%%" film))))
	(inhibit-read-only t))
    (erase-buffer)
    (imdb-kill)
    (setq films (imdb-mode-filter films))
    (unless films
      (error "No films match %S" film))
    (dolist (film (cl-sort films '<
			   :key (lambda (e)
				  (or (cl-getf e :start-year)
				      most-positive-fixnum))))
      (insert
       (propertize
	(format "%s %s%s%s%s\n"
		(imdb-face (cl-getf film :start-year))
		(propertize " " 'display '(space :align-to 6))
		(imdb-face (cl-getf film :primary-title))
		(if (equal (cl-getf film :type) "movie")
		    ""
		  (imdb-face (format " (%s)"
				     (imdb-display-type (cl-getf film :type)))
			     "#80a080"))
		(let ((directors
		       (sqorm-select-where "select primary_name from person inner join crew on crew.pid = person.pid where crew.category = 'director' and crew.mid = ?"
					   (cl-getf film :mid))))
		  (if (not directors)
		      ""
		    (imdb-face
		     (concat " " (mapconcat (lambda (e)
					      (cl-getf e :primary-name))
					    directors ", "))
		     "#80a080"))))
	'id (cl-getf film :mid))))
    (goto-char (point-min))))

(defun imdb-query-user (prompt)
  (let ((default (thing-at-point 'word)))
    (when default
      (setq default (substring-no-properties default))
      (when sqorm-regexp
	(setq default (regexp-quote default))))
    (downcase
     (read-string
      (format "%s (%s%s): "
	      prompt
	      (if sqorm-regexp
		  "regexp"
		"substring")
	      (if default
		  (format ", default %s" default)
		""))
      nil nil default))))

(defun imdb-person (person)
  "List films matching PERSON."
  (interactive (list (imdb-complete-person)))
  (imdb-initialize)
  (let ((pid (get-text-property 1 'id person)))
    (if pid
	(imdb-mode-show-person pid)
      (switch-to-buffer (format "*imdb %s*" person))
      (let ((inhibit-read-only t))
	(imdb-mode)
	(setq imdb-mode-mode 'people-search
	      imdb-mode-search person)
	(erase-buffer)
	(imdb-mode-search-person-1 person)))))

(defun imdb-mode-person-id (person)
  (caar
   (sort
    (cl-loop for (_ pid) in (sqorm-select-where
			     "select pid from person where lower(primary_name) = ?"
			     (downcase person))
	     collect (list pid (plist-get (car (sqorm-select-where
						"select count(*) from principal where pid = ?"
						pid))
					  :count)))
    (lambda (e1 e2)
      (> (nth 1 e1) (nth 1 e2))))))

(defun imdb-mode-search-person-1 (person)
  (let ((people (if sqorm-regexp
		    (sqorm-select-where
		     "select * from person where lower(primary_name) regexp ?"
		     person)
		  (sqorm-select-where
		   "select * from person where lower(primary_name) like ?"
		   (format "%%%s%%" person))))
	(inhibit-read-only t))
    (erase-buffer)
    (imdb-kill)
    (unless people
      (error "No people match %S" person))
    (dolist (person (cl-sort people 'string<
			     :key (lambda (e)
				    (cl-getf e :primary-anem))))
      (insert
       (propertize
	(format
	 "%s%s%s\n"
	 (imdb-face (cl-getf person :primary-name))
	 (if (not (cl-getf person :birth-year))
	     ""
	   (imdb-face (format " (%s)" (cl-getf person :birth-year)) "#a0a0a0"))
	 (let ((known (sqorm-select 'person-known-for
				    :pid (cl-getf person :pid))))
	   (if (not known)
	       ""
	     (imdb-face
	      (format " (%s)"
		      (mapconcat
		       (lambda (e)
			 (cl-getf (car (sqorm-select 'movie
						     :mid (cl-getf e :mid)))
				  :primary-title))
		       known
		       ", "))
	      "#a0a0a0"))))
	'id (cl-getf person :pid))))
    (goto-char (point-min))))

(defun imdb-mode-filter (films &optional pid)
  (let ((priority '("director" "writer" "producer" "actor" "editor")))
    (setq films
	  (sort
	   (copy-sequence films)
	   (lambda (f1 f2)
	     (if (string= (cl-getf f1 :mid) (cl-getf f2 :mid))
		 (< (or
		     (seq-position priority (cl-getf f1 :category))
		     -1)
		    (or
		     (seq-position priority (cl-getf f2 :category))
		     -1))
	       (< (seq-position films f1) (seq-position films f2)))))))
  (let ((films
	  (if (not imdb-mode-filter-insignificant)
	      films
	    (cl-loop for film in films
		     when (and (cl-getf film :start-year)
			       (equal (cl-getf film :type) "movie")
			       (not (member (cl-getf film :category)
					    '("thanks" "miscellaneous"
					      "camera_department"
					      "self" "archive_footage"
					      "sound_department"))))
		     collect film)))
	 (done (make-hash-table :test #'equal)))
    (if (not imdb-mode-filter-job)
	(cl-remove-duplicates films :test #'equal
			      :key (lambda (f)
				     (cl-getf f :mid))
			      :from-end t)
      (cl-loop for film in films
	       when (and
		     (not (gethash (cl-getf film :mid) done))
		     (or
		      (and (eq imdb-mode-filter-job 'acting)
			   (member (cl-getf film :category)
				   '("actor" "actress")))
		      (and (eq imdb-mode-filter-job 'directing)
			   (or
			    (equal (cl-getf film :category) "director")
			    ;; Check whether the person is listed as
			    ;; a crew member with role "director".
			    (and pid
				 (member pid
					 (seq-map
					  (lambda (elem)
					    (cl-getf elem :pid))
					  (sqorm-select-where "select pid from crew where crew.category = 'director' and crew.mid = ?"
							      (cl-getf film :mid)))))))))
			
	       collect
	       (progn
		 (setf (gethash (cl-getf film :mid) done) t)
		 film)))))

(defun imdb-mode-select ()
  "Select the item under point and display details."
  (interactive)
  (let ((id (get-text-property (point) 'id)))
    (unless id
      (error "Nothing under point"))
    (cond
     ((or (eq imdb-mode-mode 'film-search)
	  (eq imdb-mode-mode 'intersection)
	  (eq imdb-mode-mode 'person))
      (imdb-mode-show-film id))
     (t
      (imdb-mode-show-person id)))))

(defun imdb-mode-show-person (id)
  (switch-to-buffer (format "*imdb %s*"
			    (cl-getf (car (sqorm-select 'person :pid id))
				     :primary-name)))
  (let ((inhibit-read-only t))
    (imdb-mode)
    (imdb-mode-display-person id)))

(defun imdb-mode-open-imdb (all)
  "Open the item under point in a web browser."
  (interactive "P")
  (let ((urls (list (imdb-mode-get-url))))
    (when all
      (when-let ((film (and (eq imdb-mode-mode 'film)
			    (cl-getf (car (sqorm-select 'movie
							:mid imdb-mode-search))
				     :primary-title))))
	(dolist (site '("rottentomatoes.com" "wikipedia.org"))
	  (push (concat "https://www.google.com/search?q=site%3A"
			site "+"
			(string-replace " " "+" film))
		urls))))
    (open-webs urls)))

(defun imdb-mode-load-all-images ()
  "Expand all the actor images in the current buffer."
  (interactive)
  (unless (eq imdb-mode-mode 'film)
    (error "This command only works when displaying films"))
  (let ((pids nil)
	match)
  (save-excursion
    (goto-char (point-min))
    (while (setq match (text-property-search-forward 'placeholder t t))
      (push (get-text-property (prop-match-beginning match) 'id) pids)))
  (if (not pids)
      (message "Nothing to expand")
    (message "Expanding...")
    (imdb-load-people-images (nreverse pids) (current-buffer) 100 150 3))))

(defun imdb-mode-copy-url ()
  "Copy the item URL under point to the kill ring."
  (interactive)
  (let ((url (imdb-mode-get-url)))
    (with-temp-buffer
      (insert url)
      (copy-region-as-kill (point-min) (point-max))
      (message "Copied %S" (buffer-string)))))

(defun imdb-mode-get-url ()
  "Find the logical url for the current buffer/point."
  (let ((id (get-text-property (point) 'id)))
    (if (not id)
	(cond
	 ((eq imdb-mode-mode 'person)
	  (imdb-mode-person-url imdb-mode-search))
	 ((eq imdb-mode-mode 'film)
	  (imdb-mode-film-url imdb-mode-search))
	 (t
	  (error "Nothing under point")))
      (cond
       ((or (eq imdb-mode-mode 'film-search)
	    (eq imdb-mode-mode 'person))
	(imdb-mode-film-url id))
       ((or (eq imdb-mode-mode 'people-search)
	    (eq imdb-mode-mode 'film))
	(imdb-mode-person-url id))))))

(defun imdb-mode-person-url (id)
  (format "https://www.imdb.com/name/%s/" id))

(defun imdb-mode-film-url (id)
  (format "https://www.imdb.com/title/%s/" id))

(defun imdb-mode-show-film (id)
  (switch-to-buffer (format "*imdb %s*"
			    (cl-getf (car (sqorm-select 'movie :mid id))
				     :primary-title)))
  (let ((inhibit-read-only t)
	(film (car (sqorm-select 'movie :mid id))))
    (erase-buffer)
    (imdb-kill)
    (imdb-mode)
    (setq imdb-mode-mode 'film
	  imdb-mode-search id)
    (imdb-insert-placeholder 300 400)
    (insert "\n\n")
    (imdb-update-film-image id)
    (let ((directors
	   (sqorm-select-where "select primary_name, person.pid from person inner join crew on crew.pid = person.pid where crew.category = 'director' and crew.mid = ?"
			       id)))
      (insert
       (if (not directors)
	   ""
	 (concat
	  (imdb-face "Directed by ")
	  (imdb-face (mapconcat (lambda (e)
				  (propertize (cl-getf e :primary-name)
					      'id (cl-getf e :pid)))
				directors ", ")
		     '(:foreground "#f0f0f0"
				   :weight bold))))
       "\n"))

    
    (when (cl-getf film :start-year)
      (insert (imdb-face (format "%s " (cl-getf film :start-year)))))
    
    (let ((rating (car (sqorm-select 'rating :mid id))))
      (when rating
	(insert
	 (imdb-face (format "Rating %s / %s votes"
			    (cl-getf rating :rating)
			    (cl-getf rating :votes))
		    "#b0b0b0")))
      (when (cl-getf film :length)
	(when rating
	  (insert (imdb-face " / " "#b0b0b0")))
	(insert 
	 (imdb-face (format "%d mins" (cl-getf film :length)) "#b0b0b0"))))
    (insert "\n")

    (let ((genres (sqorm-select 'movie-genre :mid id)))
      (when genres
	(insert 
	 (imdb-face
	  (mapconcat (lambda (e) (cl-getf e :genre)) genres ", ")
	  "#b0b0b0")))
      (when (cl-getf film :type)
	(when genres
	  (insert (imdb-face " ")))
	(insert 
	 (imdb-face (format "(%s)" (imdb-display-type (cl-getf film :type)))
		    "#b0b0b0")))
      (when (or (cl-getf film :type)
		genres)
	(insert "\n")))

    (insert "\n")
    
    (dolist (person (cl-sort
		     (cl-delete-duplicates
		      (sqorm-select 'principal :mid id)
		      :test #'equal
		      :key (lambda (f)
			     (cl-getf f :pid)))
		     #'<
		     :key (lambda (e)
			    (let ((job (cl-getf e :category)))
			      (cond
			       ((equal job "director") 1)
			       ((equal job "actor") 2)
			       ((equal job "actress") 2)
			       ((equal job "writer") 4)
			       (t 5))))))
      (insert (propertize
	       (format
		"%s%s%s\n"
		(imdb-face
		 (cl-getf (car (sqorm-select 'person
					     :pid (cl-getf person :pid)))
			  :primary-name))
		(imdb-face
		 (format " (%s)" (imdb-display-type (cl-getf person :category)))
		 "#c0c0c0")
		(let ((characters (sqorm-select 'principal-character
						:mid id
						:pid (cl-getf person :pid))))
		  (if (not characters)
		      ""
		    (imdb-face
		     (concat
		      " "
		      (mapconcat
		       (lambda (e)
			 (format "\"%s\"" (cl-getf e :character)))
		       characters ", "))
		     "#a0a0f0"))))
	       'id (cl-getf person :pid))))
    (goto-char (point-min))
    (forward-line 1)
    (imdb-get-actors id (current-buffer))))

(defun imdb-mode-display-person (id)
  (let ((inhibit-read-only t)
	(films (sqorm-select-where
		"select movie.mid, primary_title, start_year, type, principal.category from movie inner join principal on movie.mid = principal.mid where pid = ?"
		id)))
    (erase-buffer)
    (imdb-kill)
    (imdb-insert-placeholder 300 400)
    (put-text-property (point-min) (point) 'id id)
    (insert "\n\n")
    (let ((person (car (sqorm-select 'person :pid id))))
      (insert
       (imdb-face (cl-getf person :primary-name) "#f0f0f0"))
      (when (cl-getf person :birth-year)
	(insert
	 (imdb-face (format " (%s%s)" (cl-getf person :birth-year)
			    (if (cl-getf person :death-year)
				(format "-%s" (cl-getf person :death-year))
			      ""))
		    "#a0a0a0"))))
    (when-let ((known (sqorm-select 'person-known-for :pid id)))
      (insert "\n")
      (insert (imdb-face
	       (format "Known for %s"
		       (mapconcat
			(lambda (e)
			  (propertize
			   (cl-getf (car (sqorm-select 'movie
						       :mid (cl-getf e :mid)))
				    :primary-title)
			   'id (cl-getf e :mid)))
			known
			", "))
	       "#c0c0c0")))
    (insert "\n\n")
    (imdb-load-people-images (list id) (current-buffer) 300 400 0)
    (setq imdb-mode-mode 'person
	  imdb-mode-search id)
    ;; Add the extra films from the web.
    (dolist (film imdb-mode-extra-data)
      (unless (cl-member film films
			 :test (lambda (e1 e2)
				 (equal (cl-getf e1 :mid) (cl-getf e2 :mid))))
	(push film films)))
    (setq films (cl-sort
		 (reverse films) '<
		 :key (lambda (e)
			(or (cl-getf e :start-year) 1.0e+INF))))
    (setq films (imdb-mode-filter films id))
    (dolist (film films)
      (imdb-mode-person-film film id))
    (goto-char (point-min))
    (forward-line 2)
    (imdb-person-update-films id)))

(defun imdb-mode-person-film (film pid)
  (unless (equal (cl-getf film :type) "tvEpisode") 
    (insert
     (propertize
      (format "%s %s%s%s%s%s%s\n"
	      (imdb-face (or (cl-getf film :start-year) ""))
	      (propertize " " 'display '(space :align-to 6))
	      (imdb-face (cl-getf film :primary-title))
	      (cond
	       ((equal (cl-getf film :type) "movie")
		"")
	       ((equal (cl-getf film :type) "tvSeries")
		(let ((count
		       (car
			(sqorm-select-where
			 "select count(*) from movie inner join episode on movie.mid = episode.mid inner join principal_character on principal_character.mid = movie.mid where episode.movie = ? and principal_character.pid = ?"
			 (cl-getf film :mid)
			 pid))))
		  (imdb-face
		   (if (cl-plusp (cl-getf count :count))
		       (format " (tv series, %s episode%s)"
			       (cl-getf count :count)
			       (if (> (cl-getf count :count) 1)
				   "s"
				 ""))
		     (format " (tv series)"))
		   "#a0a0a0")))
	       (t
		(imdb-face (format " (%s)" (imdb-display-type
					    (cl-getf film :type)))
			   "#a0a0a0")))
	      (imdb-face (format " (%s)" (imdb-display-type
					  (cl-getf film :category)))
			 "#c0c0c0")
	      (let ((characters (sqorm-select 'principal-character
					     :mid (cl-getf film :mid)
					     :pid pid)))
		(if (not characters)
		    ""
		  (imdb-face
		   (concat
		    " "
		    (mapconcat
		     (lambda (e)
		       (format "%S" (cl-getf e :character)))
		     characters ", "))
		   "#a0a0f0")))
	      (let ((directors
		     (sqorm-select-where "select primary_name from person inner join crew on crew.pid = person.pid where crew.category = 'director' and crew.mid = ?"
					(cl-getf film :mid))))
		(if (not directors)
		    ""
		  (imdb-face
		   (concat " " (mapconcat (lambda (e)
					    (cl-getf e :primary-name))
					  directors ", "))
		   "#80a080"))))
      'id (cl-getf film :mid)))))

(defun imdb-update-film-image (id)
  (let ((buffer (current-buffer)))
    (imdb-url-retrieve
     (imdb-mode-film-url id)
     (lambda (status)
       (goto-char (point-min))
       (if (or (not (search-forward "\n\n" nil t))
	       (cl-getf status :error))
	   (imdb-placehold-film buffer)
	 (url-store-in-cache)
	 (imdb-update-film-image-1
	  (cl-loop with dom = (libxml-parse-html-region (point) (point-max))
		   for image in (dom-by-tag dom 'meta)
		   for src = (dom-attr image 'content)
		   when (and src
			     (equal (dom-attr image 'property)
				    "og:image"))
		   return src)
	  buffer))
       (kill-buffer (current-buffer))))))

(defun imdb-placehold-film (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(save-excursion
	  (goto-char (point-min))
	  (delete-region (point) (1+ (point)))
	  (imdb-insert-placeholder 300 400 "black"))))))

(defun imdb-update-film-image-1 (url buffer)
  (if (not url)
      (imdb-placehold-film buffer)
    (imdb-url-retrieve
     url
     (lambda (status)
       (url-store-in-cache)
       (goto-char (point-min))
       (when (and (search-forward "\n\n" nil t)
		  (not (cl-getf status :error)))
	 (let ((data (buffer-substring (point) (point-max))))
	   (when (buffer-live-p buffer)
	     (with-current-buffer buffer
	       (save-excursion
		 (goto-char (point-min))
		 (let ((inhibit-read-only t))
		   (delete-region (point) (line-end-position))
		   (insert-image
		    (create-image data (imdb-mode--image-type)
				  t :height 400))))))))
       (kill-buffer (current-buffer))))))

(defun imdb-insert-placeholder (width height &optional color)
  (let* ((scale (image-compute-scaling-factor image-scaling-factor))
	 (svg (svg-create (* width scale) (* height scale))))
    (svg-gradient svg "background" 'linear '((0 . "#b0b0b0") (100 . "#808080")))
    (if color
	(svg-rectangle svg 0 0 (* width scale) (* height scale)
		       :fill-color color)
      (svg-rectangle svg 0 0 (* width scale) (* height scale)
		     :gradient "background"
                     :stroke-width 2
		     :stroke-color "black"))
    (insert-image (svg-image svg :scale 1))))

(defun imdb-clean (string)
  (string-trim (replace-regexp-in-string "[  \t\n]+" " " string)))

(defun imdb-get-actors (mid buffer)
  (imdb-url-retrieve
   (format "https://www.imdb.com/title/%s/fullcredits?ref_=tt_cl_sm" mid)
   (lambda (status)
     (goto-char (point-min))
     (when (and (search-forward "\n\n" nil t)
		(not (cl-getf status :error)))
       (url-store-in-cache)
       (let* ((table (dom-by-class
		      (libxml-parse-html-region (point) (point-max))
		      "cast_list"))
	      (people
	       (cl-loop for line in (dom-by-tag table 'tr)
			for link = (dom-by-tag line 'a)
			for person = (dom-attr link 'href)
			when (and person
				  (string-match "name/\\([^/]+\\)" person))
			collect (list :pid (match-string 1 person)
				      :name (imdb-clean
					     (dom-texts
					      (cadr (dom-non-text-children
						     line))))
				      :character (imdb-clean
						  (dom-texts
						   (dom-by-class
						    line "character"))))))
	      updates)
	 (with-current-buffer buffer
	   (save-excursion
	     (goto-char (point-max))
	     (let ((inhibit-read-only t))
	       (insert "\n")
	       (dolist (person people)
		 (let ((start (point)))
		   (if (> (length updates) 10)
		       (let ((start (point)))
			 (imdb-insert-placeholder 100 150 "#004000")
			 (put-text-property start (point) 'placeholder t))
		     (push person updates)
		     (imdb-insert-placeholder 100 150))
		   (insert
		    (format " %s%s\n"
			    (imdb-face (cl-getf person :name))
			    (if (equal (cl-getf person :character) "")
				""
			      (imdb-face (format " \"%s\""
						 (cl-getf person :character))
					 "#a0a0a0"))))
		   (put-text-property start (point)
				      'id (cl-getf person :pid)))))))
	 (kill-buffer (current-buffer))
	 (when people
	   (imdb-load-people-images
	    (mapcar (lambda (e) (cl-getf e :pid)) (nreverse updates))
	    buffer 100 150 3)))))))

(defun imdb-fetch-profile-picture (pid callback)
  (imdb-url-retrieve
   (imdb-mode-person-url pid)
   (lambda (status)
     (goto-char (point-min))
     (if (or (not (search-forward "\n\n" nil t))
	     (cl-getf status :error))
	 (progn
	   (kill-buffer (current-buffer))
	   (funcall callback nil))
       (url-store-in-cache)
       (let* ((dom (libxml-parse-html-region (point) (point-max)))
	      (img (cl-loop for elem in (dom-by-tag dom 'script)
			    for image =
			    (and (equal (dom-attr elem 'type)
					"application/ld+json")
				 (let ((json (json-parse-string
					      (car (dom-children elem)))))
				   (and json
					(gethash "image" json))))
			    when image
			    return image)))
	 (kill-buffer (current-buffer))
	 (if img
	     (imdb-url-retrieve
	      img 
	      (lambda (status)
		(goto-char (point-min))
		(if (and (search-forward "\n\n" nil t)
			 (not (cl-getf status :error)))
		    (progn
		      (url-store-in-cache)
		      (let ((data (buffer-substring (point) (point-max))))
			(kill-buffer (current-buffer))
			(funcall callback data)))
		  (kill-buffer (current-buffer))
		  (funcall callback nil))))
	   (funcall callback nil)))))))

(defun imdb-load-people-images (pids buffer width height newlines)
  (let ((pid (pop pids)))
    (imdb-fetch-profile-picture
     pid
     (lambda (image)
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer
	   (let ((inhibit-read-only t)
		 match)
	     (save-excursion
	       (goto-char (point-min))
	       (search-forward "\n\n" nil t newlines)
	       (when (setq match (text-property-search-forward
				  'id pid t))
		 (goto-char (prop-match-beginning match))
		 (let ((id (get-text-property (point) 'id))
		       (start (point)))
		   (delete-region (point) (1+ (point)))
		   (if image
		       (progn
			 (insert-image
			  (create-image image (imdb-mode--image-type) t
					:height height))
			 (put-text-property start (point) 'id id))
		     (imdb-insert-placeholder width height "black"))
		   (put-text-property start (point) 'id id))))))
	 (when pids
	   (imdb-load-people-images pids buffer width height newlines)))))))

(defun imdb-person-query-films (pid)
  "Query imdb.com for all films that PID has appeared in."
  (let ((did nil))
    (imdb-person-get-films
     pid
     (lambda (films)
       (setq did (or films t))))
    (while (not did)
      (sleep-for 0.1))
    (and (listp did)
	 did)))

(defun imdb-person-update-films (pid)
  (let ((buffer (current-buffer)))
    (imdb-person-get-films
     pid
     (lambda (films)
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer
	   (setq films (imdb-mode-filter films))
	   (setq imdb-mode-extra-data films)
	   (let ((inhibit-read-only t))
	     (save-excursion
	       (dolist (film films)
		 (goto-char (point-min))
		 (forward-line 5)
		 (unless (text-property-search-forward
			  'id (cl-getf film :mid) t)
		   (if (not (cl-getf film :start-year))
		       (goto-char (point-max))
		     (while (and (looking-at "[0-9]+")
				 (let ((year (string-to-number
					      (match-string 0))))
				   (<= year (cl-getf film :start-year))))
		       (forward-line 1)))
		   (imdb-mode-person-film film pid)))))))))))

(defun imdb-person-get-films (pid callback)
  (imdb-url-retrieve
   (imdb-mode-person-url pid)
   (lambda (_)
     (goto-char (point-min))
     (let (films)
       (when (search-forward "\n\n" nil t)
	 (url-store-in-cache)
	 (setq films
	       (cl-loop for elem in (dom-by-class
				     (libxml-parse-html-region
				      (point) (point-max))
				     "\\`filmo-row")
			for link = (dom-by-tag elem 'a)
			for href = (dom-attr link 'href)
			for character = (car (last (dom-children elem)))
			for year = (dom-by-class elem "\\`year")
			when (and href year 
				  (string-match "/title/\\([^/]+\\)" href))
			collect
			;; If we have the data on the film, use it.
			(let ((film (car (sqorm-select
					  'movie :mid (match-string 1 href)))))
			  (if film
			      (progn
				(setf (cl-getf film :category)
				      (car (split-string (dom-attr elem 'id)
							 "-")))
				film)
			    (list :mid (match-string 1 href)
				  :primary-title (imdb-clean (dom-texts link))
				  :type "movie"
				  :start-year
				  (string-to-number
				   (car (split-string
					 (imdb-clean (dom-texts year)) "/")))
				  :category
				  (car (split-string (dom-attr elem 'id) "-"))
				  :character (and (stringp character)
						  (imdb-clean character)))))))
	 (setq films (cl-sort (nreverse films) '<
			      :key (lambda (elem)
				     (or (cl-getf elem :year) 1.0e+INF)))))
       (funcall callback films)
       (kill-buffer (current-buffer))))))

(defun imdb-display-type (type)
  (pcase type
    ("tvSeries" "tv series")
    ("tvMovie" "tv movie")
    ("tvShort" "tv short")
    ("tvSpecial" "tv special")
    ("tvMiniSeries" "tv mini series")
    ("archive_footage" "footage")
    ("visual_effects" "effects")
    ("special_effects" "effects")
    ("camera_department" "camera dept")
    ("sound_department" "sound")
    ("actress" "actor")
    (_ (replace-regexp-in-string "_" " " type))))

(defvar imdb-buffers nil)

(defun imdb-url-retrieve (url callback)
  (let ((cache (url-cache-create-filename url)))
    (if (file-exists-p cache)
	(with-current-buffer (generate-new-buffer " *imdb url cache*")
	  (erase-buffer)
	  (set-buffer-multibyte nil)
	  (insert-file-contents-literally cache)
	  (funcall callback nil))
      (let ((buffer (url-retrieve url callback nil t t)))
	(push buffer imdb-buffers)))))

(defun imdb-kill ()
  (dolist (buffer imdb-buffers)
    (when (buffer-live-p buffer)
      (when (get-buffer-process buffer)
	(delete-process (get-buffer-process buffer)))
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (let ((kill-buffer-query-functions nil))
	    (kill-buffer buffer))))))
  (setq imdb-buffers nil))

(defvar imdb-minibuffer-local-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'minibuffer-complete)
    (define-key map [(meta n)] 'imdb-next-completion)
    (define-key map [(meta p)] 'imdb-prev-completion)
    map)
  "Local keymap for minibuffer input with completion.")

(defun imdb-next-completion ()
  (interactive)
  (let ((buffer (current-buffer)))
    (when (get-buffer "*Completions*")
      (set-buffer "*Completions*")
      (next-completion 1)
      (imdb-insert-completion buffer))))

(defun imdb-insert-completion (buffer)
  (set-window-point (get-buffer-window) (point))
  (let ((string
	 (let (beg end)
           (when
	       (cond ((and (not (eobp)) (get-text-property (point) 'mouse-face))
		      (setq end (point) beg (1+ (point))))
		     ((and (not (bobp))
			   (get-text-property (1- (point)) 'mouse-face))
		      (setq end (1- (point)) beg (point))))
	     (setq beg (previous-single-property-change beg 'mouse-face))
	     (setq end (or (next-single-property-change end 'mouse-face)
			   (point-max)))
	     (buffer-substring beg end)))))
    (set-buffer buffer)
    (delete-region (minibuffer-prompt-end) (point-max))
    (insert string)))

(defun imdb-prev-completion ()
  (interactive)
  (let ((buffer (current-buffer)))
    (when (get-buffer "*Completions*")
      (set-buffer "*Completions*")
      (previous-completion 1)
      (imdb-insert-completion buffer))))

(defun imdb-completing-read (prompt collection)
  (let ((completion-in-region-function
	 (lambda (start end _ &optional __)
	   (let ((string (buffer-substring start end)))
	     (when (> (length string) 2)
	       (imdb-complete string collection)))))
	(minibuffer-allow-text-properties t))
    (read-from-minibuffer prompt nil imdb-minibuffer-local-completion-map)))

(defun imdb-complete (string collection)
  (let ((try (funcall collection string nil)))
    (cond
     ((null try)
      )
     ((consp try)
      (delete-region (minibuffer-prompt-end)
		     (point-max))
      (insert (cdr try)))
     (t
      (if (equal try string)
	  (imdb-complete-show-matches string collection)
	(delete-region (minibuffer-prompt-end)
		       (point-max))
	(if (eq try t)
	    (insert string)
	  (insert try)))))))

(defvar imdb-completion-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'imdb-choose-completion)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [down-mouse-2] nil)
    (define-key map "\C-m" 'imdb-choose-completion)
    (define-key map "\e\e\e" 'delete-completion-window)
    (define-key map [left] 'previous-completion)
    (define-key map [right] 'next-completion)
    (define-key map [?\t] 'next-completion)
    (define-key map [backtab] 'previous-completion)
    (define-key map "q" 'quit-window)
    (define-key map "z" 'kill-current-buffer)
    map)
  "Local map for completion list buffers.")

(defun imdb-complete-show-matches (string collection)
  (let ((completion-list-insert-choice-function
	 (lambda (_beg _end newtext)
	   (delete-region (minibuffer-prompt-end) (point-max))
	   (insert newtext)))
	(completion-list-mode-map imdb-completion-list-mode-map))
    (when (buffer-live-p "*Completions*")
      (kill-buffer "*Completions*"))
    (with-current-buffer-window
     "*Completions*"
     ;; This is a copy of `display-buffer-fallback-action'
     ;; where `display-buffer-use-some-window' is replaced
     ;; with `display-buffer-at-bottom'.
     `((display-buffer--maybe-same-window
	display-buffer-reuse-window
	display-buffer--maybe-pop-up-frame
	;; Use `display-buffer-below-selected' for inline completions,
	;; but not in the minibuffer (e.g. in `eval-expression')
	;; for which `display-buffer-at-bottom' is used.
	,(if (eq (selected-window) (minibuffer-window))
             'display-buffer-at-bottom
           'display-buffer-below-selected))
       ,(if temp-buffer-resize-mode
	    '(window-height . resize-temp-buffer-window)
	  '(window-height . fit-window-to-buffer))
       ,(when temp-buffer-resize-mode
	  '(preserve-size . (nil . t))))
     nil
     (display-completion-list (funcall collection string t)))))

(defun imdb-complete-person ()
  (imdb-initialize)
  (imdb-completing-read "Person: " 'imdb-complete-person-1))

(defun imdb-complete-person-1 (string flag)
  (cond
   ;; try-completion
   ((null flag)
    (let ((matches (sqorm-find 'person-search :person-search '=
			       (format "%s*" string))))
      (cond
       ((and (= (length matches) 1)
	     (cl-search (downcase string)
			(downcase (cl-getf (car matches) :primary-name))))
	(cons
	 t
	 (propertize (cl-getf (car matches) :primary-name)
		     'id (cl-getf (car matches) :pid))))
       ((null matches)
	nil)
       (t
	(let ((try
	       (try-completion
		(downcase string)
		(cl-loop for e in matches
			 collect (substring    
				  (downcase (cl-getf e :primary-name))
				  (cl-search
				   (downcase string)
				   (downcase (cl-getf e :primary-name))))))))
	  (if (eq try t)
	      (downcase string)
	    try))))))
   ;; all-completions
   ((eq flag t)
    (imdb-highlight-match
     string
     (imdb-sort-people-completions
      (cl-loop for e in (sqorm-find 'person-search :person-search '=
				    (format "%s*" string))
	       collect (propertize (cl-getf e :primary-name)
				   'id (cl-getf e :pid))))))
   (t
    nil)))

(defun imdb-highlight-match (match strings)
  (dolist (string strings)
    (when-let ((start (cl-search (downcase match) (downcase string))))
      (put-text-property start (+ start (length match)) 'face 'underline
			 string)))
  strings)

(defun imdb-sort-people-completions (completions)
  (cl-sort completions '>
	   :key (lambda (e)
		  (or (cl-getf (car (sqorm-select-where
				     "select count(*) from principal inner join movie on movie.mid = principal.mid where pid = ? and category in ('actor', 'actress', 'director') and movie.type = 'movie'"
				     (get-text-property 1 'id e)))
			       :count)
		      0))))

(defun imdb-complete-film ()
  (imdb-initialize)
  (imdb-completing-read "Film: " 'imdb-complete-film-1))

(defun imdb-complete-film-1 (string flag)
  (cond
   ;; try-completion
   ((null flag)
    (let ((matches (sqorm-find 'movie-search :movie-search '=
			       (format "%s*" string))))
      (cond
       ((and (= (length matches) 1)
	     (cl-search (downcase string)
			(downcase (cl-getf (car matches) :primary-title))))
	(cons
	 t
	 (propertize (cl-getf (car matches) :primary-title)
		     'id (cl-getf (car matches) :mid))))
       ((null matches)
	nil)
       (t
	(try-completion
	 (downcase string)
	 (cl-loop for e in matches
		  collect (substring    
			   (downcase (cl-getf e :primary-title))
			   (cl-search
			    (downcase string)
			    (downcase (cl-getf e :primary-title))))))))))
   ;; all-completions
   ((eq flag t)
    (imdb-highlight-match
     string
     (imdb-sort-film-completions
      (cl-loop for e in (sqorm-find 'movie-search :movie-search '=
				    (format "%s*" string))
	       collect (propertize (cl-getf e :primary-title)
				   'id (cl-getf e :mid))))))
   (t
    nil)))

(defun imdb-sort-film-completions (completions)
  (cl-sort completions '>
	   :key (lambda (e)
		  (or (cl-getf (car (sqorm-select-where
				     "select votes from rating where mid = ?"
				     (get-text-property 1 'id e)))
			       :votes)
		      0))))

(defun imdb-populate-search ()
  (ignore-errors
    (sqlite-execute imdb-db "drop table person_search"))
  (sqlite-execute
   imdb-db
   "create virtual table person_search USING fts5 (primary_name, pid)")
  (sqlite-transaction imdb-db)
  (let* ((pids (sqorm-select-where "select pid from principal where category in ('actor', 'actress', 'director') group by pid having count(mid) > 10"))
	 (lines 1)
	 (total (length pids)))
    (dolist (pid pids)
      (when (zerop (% (cl-incf lines) 1000))
	(message "Read %d lines (%.1f%%)" lines
		 (* (/ (* lines 1.0) total) 100)))
      (let ((name (cl-getf (car (sqorm-select 'person :pid (cl-getf pid :pid)))
			   :primary-name)))
	(sqorm-insert (imdb-make 'person-search
				 (list name (cl-getf pid :pid)))))))
  (sqlite-commit imdb-db)

  (ignore-errors
    (sqlite-execute imdb-db "drop table movie_search"))
  (sqlite-execute
   imdb-db
   "create virtual table movie_search USING fts5 (primary_title, mid)")
  (sqlite-transaction imdb-db)
  (let* ((films (sqorm-select-where "select primary_title, movie.mid from movie inner join rating on rating.mid = movie.mid and votes > 100"))
	 (lines 1)
	 (total (length films)))
    (dolist (film films)
      (when (zerop (% (cl-incf lines) 1000))
	(message "Read %d lines (%.1f%%)" lines
		 (* (/ (* lines 1.0) total) 100)))
      (sqorm-insert (imdb-make 'movie-search
			       (list (cl-getf film :primary-title)
				     (cl-getf film :mid))))))
  (sqlite-commit imdb-db))

(defun imdb-choose-completion (&optional event)
  "Choose the completion at point.
If EVENT, use EVENT's position to determine the starting position."
  (interactive (list last-nonmenu-event))
  ;; In case this is run via the mouse, give temporary modes such as
  ;; isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (let ((buffer completion-reference-buffer)
          (base-position completion-base-position)
          (insert-function completion-list-insert-choice-function)
          (choice
           (save-excursion
             (goto-char (posn-point (event-start event)))
             (let (beg end)
               (cond
                ((and (not (eobp)) (get-text-property (point) 'mouse-face))
                 (setq end (point) beg (1+ (point))))
                ((and (not (bobp))
                      (get-text-property (1- (point)) 'mouse-face))
                 (setq end (1- (point)) beg (point)))
                (t (error "No completion here")))
               (setq beg (previous-single-property-change beg 'mouse-face))
               (setq end (or (next-single-property-change end 'mouse-face)
                             (point-max)))
               (buffer-substring beg end)))))

      (unless (buffer-live-p buffer)
        (error "Destination buffer is dead"))
      (quit-window nil (posn-window (event-start event)))

      (with-current-buffer buffer
        (choose-completion-string
         choice buffer
         (or base-position
             ;; If all else fails, just guess.
             (list (choose-completion-guess-base-position choice)))
         insert-function)))))

(defun imdb-mode--image-type ()
  (if (or (and (fboundp 'image-scaling-p)
	       (image-scaling-p))
	  (not (fboundp 'imagemagick-types)))
      nil
    'imagemagick))

(provide 'imdb-mode)

;;; imdb-mode.el ends here
