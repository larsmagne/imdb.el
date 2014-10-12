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
	  (shr-transform-dom (libxml-parse-xml-region (point) (point-max))))
      (kill-buffer (current-buffer)))))

(defun imdb-sort-results (dom)
  (sort (dom-by-name dom 'ImdbEntity)
	(lambda (n1 n2)
	  (< (imdb-rank dom n1) (imdb-rank dom n2)))))

(defun imdb-filter-results (movies)
  "Filter out all shorts and entries without a year."
  (loop for movie in movies
	for description = (dom-text (dom-by-name movie 'Description))
	when (and (not (string-match "\\bshort\\b" description))
		  (string-match "^[0-9][0-9][0-9][0-9]" description)
		  (dom-by-name movie 'a))
	collect movie))

(defun imdb-rank (dom node)
  (if (equal (dom-attr (dom-parent dom node) :type) "title_exact")
      1
    2))

(defun imdb-extract-data (results)
  (loop for i from 0
	for node in results
	collect (format
		 "%s%s, %s, %s"
		 (if (< i 5)
		     (or (imdb-get-image (dom-attr node :id)) "")
		   "")
		 (replace-regexp-in-string
		  "[^0-9]+" ""
		  (dom-text (dom-by-name node 'Description)))
		 (replace-regexp-in-string
		  "," "" (dom-text (dom-by-name node 'a)))
		 (dom-text node))))

(defun imdb-get-image (id)
  (with-current-buffer (url-retrieve-synchronously
			(format "http://www.imdb.com/title/%s/" id))
    (goto-char (point-min))
    (prog1
	(when (search-forward "\n\n" nil t)
	  (loop for image in (dom-by-name
			      (shr-transform-dom (libxml-parse-html-region
						  (point) (point-max)))
			      'img)
		for src = (dom-attr image :src)
		when (string-match "SY317" src)
		return (imdb-get-image-string src)))
      (kill-buffer (current-buffer)))))

(defun imdb-get-image-string (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (prog1
	(when (search-forward "\n\n" nil t)
	  (propertize
	   "foo"
	   'display
	   (create-image (buffer-substring (point) (point-max)) nil t)))
      (kill-buffer (current-buffer)))))

(defun imdb-query (title)
  "Query IMDB for TITLE, and then prompt the user for the right match."
  (interactive "sTitle: ")
  (let* ((data (imdb-extract-data
		(imdb-filter-results
		 (imdb-sort-results (imdb-get-data title)))))
	 (result (completing-read "Movie: " (cdr data) nil nil (car data))))
    (when (string-match "\\([^,]+\\), *\\([^,]+\\)" result)
      (list (match-string 1 result)
	    (match-string 2 result)))))

(provide 'imdb)

;;; imdb.el ends here

