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

(defvar imdb-query-url "http://www.imdb.com/xml/find?xml=1&nr=1&tt=on&q=%s")

(defun imdb-get-data (title)
  (with-current-buffer (url-retrieve-synchronously
			(format imdb-query-url title))
    (goto-char (point-min))
    (prog1
	(when (re-search-forward "\n\n" nil t)
	  (libxml-parse-xml-region (point) (point-max)))
      (kill-buffer (current-buffer)))))

(defun imdb-sort-results (dom)
  (sort (dom-by-tag dom 'ImdbEntity)
	(lambda (n1 n2)
	  (< (imdb-rank dom n1) (imdb-rank dom n2)))))

(defun imdb-filter-results (movies)
  "Filter out all shorts and entries without a year."
  (loop for movie in movies
	for description = (dom-text (dom-by-tag movie 'Description))
	when (and (not (string-match "\\bshort\\b" description))
		  (string-match "^[0-9][0-9][0-9][0-9]" description))
	collect movie))

(defun imdb-rank (dom node)
  (if (equal (dom-attr (dom-parent dom node) 'type) "title_exact")
      1
    2))

(defun imdb-extract-data (results)
  (loop for i from 0
	for node in results
	for data = (imdb-get-image-and-country (dom-attr node 'id))
	while (< i 5)
	collect (format
		 "%s%s, %s, %s, %s, %s"
		 (if (< i 5)
		     (or (car data) "")
		   "")
		 (replace-regexp-in-string
		  "[^0-9]+" ""
		  (dom-text (dom-by-tag node 'Description)))
		 (cadr data)
		 (dom-attr node 'id)
		 (replace-regexp-in-string
		  "," "" (dom-text (dom-by-tag node 'a)))
		 (dom-text node))))

(defun imdb-get-image-and-country (id)
  (with-current-buffer (url-retrieve-synchronously
			(format "http://www.imdb.com/title/%s/" id))
    (goto-char (point-min))
    (let ((country (save-excursion
		     (when (re-search-forward "countries=\\([a-z]+\\)" nil t)
		       (match-string 1)))))
      (prog1
	  (when (search-forward "\n\n" nil t)
	    (loop for image in (dom-by-tag
				(libxml-parse-html-region (point) (point-max))
				'img)
		  for src = (dom-attr image 'src)
		  when (and src (string-match "_AL_" src))
		  return (list (imdb-get-image-string src)
			       country)))
	(kill-buffer (current-buffer))))))

(defun imdb-get-image-string (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (prog1
	(when (search-forward "\n\n" nil t)
	  (propertize
	   " "
	   'display
	   (create-image (buffer-substring (point) (point-max)) nil t)))
      (kill-buffer (current-buffer)))))

(defun imdb-query-full (title)
  (loop for result in (imdb-extract-data
		       (imdb-filter-results
			(imdb-sort-results (imdb-get-data title))))
	when (string-match
	      " *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\)"
	      result)
	collect (list :year (match-string 1 result)
		      :director (match-string 4 result)
		      :country (match-string 2 result)
		      :title (match-string 5 result)
		      :id (match-string 3 result))))

(defun imdb-query (title)
  "Query IMDB for TITLE, and then prompt the user for the right match."
  (interactive "sTitle: ")
  (let* ((data (imdb-extract-data
		(imdb-filter-results
		 (imdb-sort-results (imdb-get-data title)))))
	 (result (completing-read "Movie: " (cdr data) nil nil (car data))))
    (when (string-match " *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\)," result)
      (list :year (match-string 1 result)
	    :director (match-string 4 result)
	    :country (match-string 2 result)
	    :id (match-string 3 result)))))

(provide 'imdb)

;;; imdb.el ends here
