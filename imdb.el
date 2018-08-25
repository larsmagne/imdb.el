;;; imdb.el --- querying the imdb movie database
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

(require 'cl)
(require 'url)
(require 'dom)
(require 'json)

(defvar imdb-query-url "http://www.imdb.com/find?q=%s&s=tt&ref_=fn_al_tt_mr")

(defun imdb-get-data (title)
  (with-current-buffer (url-retrieve-synchronously
			(format imdb-query-url
				(replace-regexp-in-string "&" "%26" title)))
    (goto-char (point-min))
    (prog1
	(when (re-search-forward "\n\n" nil t)
	  (libxml-parse-html-region (point) (point-max)))
      (kill-buffer (current-buffer)))))

(defun imdb-get-image-and-country (id &optional image-only)
  (with-current-buffer (url-retrieve-synchronously
			(format "http://www.imdb.com/title/%s/" id))
    (goto-char (point-min))
    (let ((country (save-excursion
		     (when (re-search-forward
			    "country_of_origin=\\([a-z]+\\)" nil t)
		       (match-string 1)))))
      (prog1
	  (when (search-forward "\n\n" nil t)
	    (loop with dom = (libxml-parse-html-region (point) (point-max))
		  for image in (dom-by-tag dom 'img)
		  for src = (dom-attr image 'src)
		  when (and src (string-match "_AL_" src))
		  return (if image-only
			     (imdb-get-image
			      (shr-expand-url
			       (dom-attr (dom-parent dom image) 'href)
			       "http://www.imdb.com/"))
			   (list (imdb-get-image-string src)
				 country
				 (loop for link in (dom-by-tag dom 'a)
				       for href = (dom-attr link 'href)
				       when (and href
						 (string-match "ref_=tt_ov_dr$"
							       href))
				       return (dom-texts link))))))
	(kill-buffer (current-buffer))))))

(defun imdb-get-image-string (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (prog1
	(when (search-forward "\n\n" nil t)
	  (let ((image
		 (ignore-errors
		   (create-image
		    (buffer-substring (point) (point-max)) nil t))))
	    (when image
	      (propertize
	       " "
	       'display image))))
      (kill-buffer (current-buffer)))))

(defun imdb-get-image (url)
  (let* ((json (imdb-get-image-json url))
	 (src (imdb-get-image-from-json json)))
    (with-current-buffer (url-retrieve-synchronously src)
      (goto-char (point-min))
      (prog1
	  (when (search-forward "\n\n" nil t)
	    (buffer-substring (point) (point-max)))
	(kill-buffer (current-buffer))))))

(defun imdb-get-image-from-json (json)
  (let* ((images
	  (cdr (cadr (cadr (assq 'galleries (assq 'mediaviewer json))))))
	 (aax
	  (cdr
	   (assq 'aaxUrl
		 (cdr
		  (assq 'interstitialModel
			(cadr (assq 'galleries
				    (assq 'mediaviewer json))))))))
	 ;; The default (and most "important") poster is named in a
	 ;; string in the "aax" element.  *sigh*
	 (initial (and (string-match "mediaviewer%2F\\([^%]+\\)" aax)
		       (match-string 1 aax))))
    (loop for image across images
	  when (equal (cdr (assq 'id image)) initial)
	  return (cdr (assq 'src image)))))

;; The images that IMDB displays for a movie are encoded in a
;; Javascript array (which isn't valid JSON) inside some more JS.
;; This will probably stop working when IMDB change...  whatever.
(defun imdb-get-image-json (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (prog1
	(when (and (search-forward "\n\n" nil t)
		   (search-forward "window.IMDbReactInitialState.push("))
	  (delete-region (point-min) (point))
	  (end-of-line)
	  (search-backward "}")
	  (forward-char 1)
	  (delete-region (point) (point-max))
	  (goto-char (point-min))
	  (while (re-search-forward "'" nil t)
	    (replace-match "\"" t t))
	  (goto-char (point-min))
	  (json-read))
      (kill-buffer (current-buffer)))))

(defun imdb-query-full (title)
  (loop for result in (imdb-extract-data
		       (imdb-get-data title))
	when (string-match
	      " *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\)"
	      result)
	collect (list :year (match-string 1 result)
		      :director (match-string 4 result)
		      :country (match-string 2 result)
		      :title (match-string 5 result)
		      :id (match-string 3 result))))

(defun imdb-extract-data (dom)
  (loop for i from 0
	for elem in (dom-by-class dom "findResult")
	for links = (dom-by-tag elem 'a)
	for id = (let ((href (dom-attr (car links) 'href)))
		   (when (string-match "/title/\\([^/]+\\)" href)
		     (match-string 1 href)))
	while (< i 10)
	for data = (imdb-get-image-and-country id)
	for year = (let ((text (dom-texts elem)))
		     (when (string-match "(\\([0-9][0-9][0-9][0-9]\\))" text)
		       (match-string 1 text)))
	collect (format
		 "%s%s, %s, %s, %s, %s"
		 (or (car data) "")
		 year
		 (cadr data)
		 id
		 (or (caddr data) "")
		 (dom-text (cadr links))
		 "")))

(defun imdb-query (title)
  "Query IMDB for TITLE, and then prompt the user for the right match."
  (interactive "sTitle: ")
  (let* ((data (imdb-extract-data
		(imdb-get-data title)))
	 (result (completing-read "Movie: " (cdr data) nil nil (car data))))
    (when (string-match " *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\)," result)
      (list :year (match-string 1 result)
	    :country (match-string 2 result)
	    :id (match-string 3 result)
	    :director (match-string 4 result)))))

(defun imdb-download-data ()
  (let ((dom
	 (with-current-buffer (url-retrieve-synchronously "https://datasets.imdbws.com/")
	   (goto-char (point-min))
	   (search-forward "\n\n")
	   (prog1
	       (libxml-parse-html-region (point) (point-max))
	     (kill-buffer (current-buffer))))))
    (loop for elem in (dom-by-tag dom 'a)
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
				  "~/.emacs.d/imdb")))
      (unless (file-exists-p (file-name-directory file))
	(make-directory (file-name-directory file) t))
      (write-region (point) (point-max) file))
    (kill-buffer (current-buffer))))

(defvar imdb-data-people (make-hash-table :test #'equal))
(defvar imdb-data-films (make-hash-table :test #'equal))
(defvar imdb-data-participants (make-hash-table :test #'equal))
(defvar imdb-data-participated-in (make-hash-table :test #'equal))

(defun imdb-read-data ()
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/imdb/name.basics.tsv")
    (forward-line 1)
    (while (re-search-forward "^\\([^\t]*\\)\t\\([^\t]*\\)\t\\([^\t]*\\)" nil t)
      (setf (gethash (match-string 1) imdb-data-people)
	    (list (match-string 2)
		  (match-string 3)))))
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/imdb/title.basics.tsv")
    (forward-line 1)
    (while (re-search-forward "^\\([^\t]*\\)\t\\([^\t]*\\)\t\\([^\t]*\\)\t[^\t]*\t[^\t]*\t\\([^\t]*\\)\t" nil t)
      (setf (gethash (match-string 1) imdb-data-films)
	    (list (match-string 3)
		  (if (equal (match-string 4) "\\N")
		      "?"
		    (match-string 4))
		  (match-string 2)))))
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/imdb/title.principals.tsv")
    (forward-line 1)
    (while (re-search-forward "^\\([^\t]*\\)\t[^\t]*\t\\([^\t]*\\)\t\\([^\t]*\\)\t[^\t]*\t\\([^\t\n]*\\)" nil t)
      (push (list (match-string 2)
		  (match-string 3)
		  (match-string 4))
	    (gethash (match-string 1) imdb-data-participants nil))
      (push (list (match-string 1)
		  (match-string 3)
		  (match-string 4))
	    (gethash (match-string 2) imdb-data-participated-in nil)))))

(defvar imdb-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "f" 'imdb-mode-search-film)
    (define-key map "a" 'imdb-mode-search-actor)
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
      (insert (propertize (format "%04s  %s%s\n"
				  (nth 2 film)
				  (nth 1 film)
				  (if (equal (nth 3 film) "movie")
				      ""
				    (format " (%s)" (nth 3 film))))
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
	  (eq imdb-mode-mode 'person-search)
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
    (dolist (participant (reverse (gethash id imdb-data-participants)))
      (insert (propertize (format "%s (%s)%s\n"
				  (car (gethash (car participant)
						imdb-data-people))
				  (nth 1 participant)
				  (if (equal (nth 2 participant) "\\N")
				      ""
				    (format " %s" (nth 2 participant))))
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
      (insert (propertize
	       (format "%s %s%s%s%s%s\n"
		       (propertize (nth 2 film) 'face 'variable-pitch)
		       (propertize " " 'display '(space :align-to 8))
		       (propertize (nth 1 film) 'face 'variable-pitch)
		       (if (equal (nth 3 film) "movie")
			   ""
			 (propertize (format " (%s)" (nth 3 film))
				     'face '(variable-pitch
					     (:foreground "#a0a0a0"))))
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
	       'id (car film))))
    (goto-char (point-min))))

(provide 'imdb)

;;; imdb.el ends here
