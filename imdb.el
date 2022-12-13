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

(require 'cl-lib)
(require 'url)
(require 'dom)
(require 'json)

(defvar imdb-query-url "https://www.imdb.com/find?q=%s&s=tt&ref_=fn_al_tt_mr")

(defun imdb-url-retrieve-synchronously (url)
  (let ((cache (url-cache-create-filename url)))
    (if (file-exists-p cache)
	(with-current-buffer (generate-new-buffer " *imdb url cache*")
	  (erase-buffer)
	  (set-buffer-multibyte nil)
	  (insert-file-contents-literally cache)
	  (current-buffer))
      (url-retrieve-synchronously url))))

(defun imdb-get-data (title)
  (with-current-buffer (imdb-url-retrieve-synchronously
			(format imdb-query-url
				(replace-regexp-in-string "&" "%26" title)))
    (url-store-in-cache)
    (goto-char (point-min))
    (prog1
	(when (re-search-forward "\n\n" nil t)
	  (libxml-parse-html-region (point) (point-max)))
      (kill-buffer (current-buffer)))))

(defun imdb-get-image-and-country (id &optional image-only just-image)
  (with-current-buffer (imdb-url-retrieve-synchronously
			(format "https://www.imdb.com/title/%s/" id))
    (url-store-in-cache)
    (goto-char (point-min))
    (let ((country (save-excursion
		     (when (re-search-forward
			    "country_of_origin=\\([a-zA-Z]+\\)" nil t)
		       (match-string 1)))))
      (prog1
	  (when (search-forward "\n\n" nil t)
	    (cl-loop with dom = (libxml-parse-html-region (point) (point-max))
		     for image in (dom-by-tag dom 'meta)
		     for src = (dom-attr image 'content)
		     when (and src
			       (equal (dom-attr image 'property)
				      "og:image"))
		     return
		     (if image-only
			 (imdb-get-image src)
		       (if just-image
			   (imdb-get-image-data src)
			 (list (imdb-get-image-string src)
			       country
			       ;; Director.
			       (cl-loop for link in (dom-by-tag dom 'li)
					for span = (dom-by-tag link 'button)
					when (and span
						  (equal (dom-text span)
							 "Director"))
					return (dom-texts
						(dom-by-tag link 'a))))))))
	(kill-buffer (current-buffer))))))

(defun imdb-get-image-string (url)
  (with-current-buffer (imdb-url-retrieve-synchronously url)
    (url-store-in-cache)
    (goto-char (point-min))
    (prog1
	(when (search-forward "\n\n" nil t)
	  (let ((image
		 (ignore-errors
		   (create-image
		    (buffer-substring (point) (point-max)) nil t
		    :max-height 200))))
	    (when image
	      (propertize
	       " "
	       'display image))))
      (kill-buffer (current-buffer)))))

(defun imdb-get-image-data (url)
  (with-current-buffer (imdb-url-retrieve-synchronously url)
    (url-store-in-cache)
    (goto-char (point-min))
    (prog1
	(when (search-forward "\n\n" nil t)
	  (buffer-substring (point) (point-max)))
      (kill-buffer (current-buffer)))))

(defun imdb-get-image (url)
  (let* ((json (imdb-get-image-json url))
	 (src (imdb-get-image-from-json json)))
    (when src
      (with-current-buffer (imdb-url-retrieve-synchronously src)
	(url-store-in-cache)
	(goto-char (point-min))
	(prog1
	    (when (search-forward "\n\n" nil t)
	      (buffer-substring (point) (point-max)))
	  (kill-buffer (current-buffer)))))))

(defun imdb-get-image-from-json (json)
  (if (listp json)
      (let* ((images
	      (cdr (assq 'allImages
			 (cadr (assq 'galleries (assq 'mediaviewer json))))))
	     (aax
	      (cdr
	       (assq 'aaxUrl
		     (cdr
		      (assq 'interstitialModel
			    (cadr (assq 'galleries
					(assq 'mediaviewer json))))))))
	     ;; The default (and most "important") poster is named in a
	     ;; string in the "aax" element.  *sigh*
	     (initial (and aax
			   (string-match "mediaviewer%2F\\([^%]+\\)" aax)
			   (match-string 1 aax))))
	(cl-loop for image across images
		 when (equal (cdr (assq 'id image)) initial)
		 return (cdr (assq 'src image))))
    ;; This used to be much more complicated, but now it's just the
    ;; first image in the list.  But retain the loop just because
    ;; that'll change.
    (cl-loop for image across json
	     return (cdr (assq 'url (cdr (assq 'node image)))))))

(defun imdb-get-image-json (url)
  (with-current-buffer (imdb-url-retrieve-synchronously url)
    (url-store-in-cache)
    (goto-char (point-min))
    (prog1
	(imdb-extract-image-json)
      (kill-buffer (current-buffer)))))

;; The images that IMDB displays for a movie are encoded in a
;; Javascript array (which isn't valid JSON) inside some more JS.
;; This will probably stop working when IMDB change...  whatever.
(defun imdb-extract-image-json ()
  (when (search-forward "\n\n" nil t)
    (cond
     ((search-forward "window.IMDbMediaViewerInitialState = " nil t)
      (delete-region (point-min) (point))
      (end-of-line)
      (search-backward "}")
      (forward-char 1)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (when (re-search-forward "'mediaviewer'" nil t)
	(replace-match "\"mediaviewer\"" t t)))
     ((search-forward "[{\"position\":" nil t)
      (delete-region (point-min) (match-beginning 0))))
    (goto-char (point-min))
    (json-read)))

(defun imdb-query-full (title)
  (cl-loop for result in (imdb-extract-data
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
  (cl-loop for i from 0
	   for elem in (dom-by-class dom "ipc-metadata-list-summary-item__c\\'")
	   for links = (dom-by-tag elem 'a)
	   for id = (let ((href (dom-attr (car links) 'href)))
		      (when (string-match "/title/\\([^/]+\\)" href)
			(match-string 1 href)))
	   while (< i 10)
	   for data = (imdb-get-image-and-country id)
	   for year = (dom-text (dom-by-class elem "ipc-metadata-list-summary-item__li"))
	   collect (format
		    "%s%s, %s, %s, %s, %s"
		    (or (car data) "")
		    year
		    (cadr data)
		    id
		    (or (caddr data) "")
		    (dom-texts elem)
		    "")))

(defun imdb-query (title)
  "Query IMDB for TITLE, and then prompt the user for the right match."
  (interactive "sTitle: ")
  (let* ((max-mini-window-height 0.5)
	 (data (imdb-extract-data (imdb-get-data title)))
	 (result (if data
		     (completing-read "Movie: " (cdr data) nil nil
				      (cons (car data) 0))
		   (completing-read "Movie: " nil))))
    (when (string-match " *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\)," result)
      (list :year (match-string 1 result)
	    :country (match-string 2 result)
	    :id (match-string 3 result)
	    :director (match-string 4 result)))))

(provide 'imdb)

;;; imdb.el ends here
