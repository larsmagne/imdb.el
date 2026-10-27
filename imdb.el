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
(require 'url-cache)
(require 'fetch-dom)

(defvar imdb-query-url "https://www.imdb.com/find/?q=%s&ref_=hm_nv_srb_sm")

(defun imdb-fetch-url (url)
  (let ((default-directory (file-name-directory (locate-library "imdb"))))
    (with-current-buffer (generate-new-buffer " *imdb url cache*")
      (let ((cache (url-cache-create-filename url)))
	(if (file-exists-p cache)
	    (insert-file-contents cache)
	  (insert (fetch-dom url :type 'string
			     :min-level 'headless
			     :ignore-previous-failures t))
          (let ((coding-system-for-write 'binary))
	    (unless (file-exists-p (file-name-directory cache))
	      (make-directory (file-name-directory cache) t))
            (write-region (point-min) (point-max) cache nil 'silent))))
      (goto-char (point-min))
      (current-buffer))))

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
  (with-current-buffer (imdb-fetch-url
			(format imdb-query-url (browse-url-encode-url title)))
    (prog1
	(libxml-parse-html-region (point) (point-max))
      (kill-buffer (current-buffer)))))

(defun imdb--get-poster (dom)
  ;; The primary poster image is in a JSON structure in a script.
  (let ((elem (dom-text (dom-by-id dom "__NEXT_DATA__"))))
    (when (> (length elem) 0)
      (let ((json (with-temp-buffer
		    (insert elem)
		    (goto-char (point-min))
		    (json-parse-buffer :object-type 'plist))))
	(plist-get
	 (plist-get
	  (plist-get (plist-get (plist-get json :props) :pageProps)
		     :aboveTheFoldData)
	  :primaryImage)
	 :url)))))

(defun imdb-get-image-and-country (id &optional image-only no-string)
  (with-current-buffer (imdb-fetch-url
			(format "https://www.imdb.com/title/%s/" id))
    (let ((country (save-excursion
		     (when (re-search-forward
			    "country_of_origin=\\([a-zA-Z]+\\)" nil t)
		       (match-string 1)))))
      (prog1
	  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
	    (when-let ((src (imdb--get-poster dom)))
	      (cond
	       (image-only
		(imdb-get-image src))
	       (no-string
		(list (imdb-get-image src) country
		      (string-join (imdb--dom-director dom) ", ")))
	       (t
		(list (imdb-get-image-string src)
		      country
		      (string-join (imdb--dom-director dom) " + "))))))
	(kill-buffer (current-buffer))))))


(defun imdb--dom-director (dom)
  (cl-loop for link in (dom-by-tag dom 'li)
	   for span = (dom-by-tag link 'span)
	   when (and span
		     (or (equal (dom-text span)
				"Director")
			 (equal (dom-text span)
				"Directors")))
	   return
	   (cl-loop for dir in (dom-by-tag link 'a)
		    collect (dom-text dir))))

(defun imdb-get-image-string (url)
  (propertize
   " " 'display (create-image (imdb-get-image url) nil t :max-height 200)))

(defun imdb-get-image (url)
  (with-current-buffer (imdb-url-retrieve-synchronously url)
    (url-store-in-cache)
    (goto-char (point-min))
    (prog1
	(when (search-forward "\n\n" nil t)
	  (buffer-substring (point) (point-max)))
      (kill-buffer (current-buffer)))))

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

(defun imdb-extract-data (dom &optional max-results)
  (cl-loop for i from 0
	   for elem in (dom-by-class dom "ipc-metadata-list-summary-item__c\\'")
	   for links = (dom-by-tag elem 'a)
	   for id = (let ((href (dom-attr (car links) 'href)))
		      (when (string-match "/title/\\([^/]+\\)" href)
			(match-string 1 href)))
	   for year = (dom-text (dom-by-class elem "ipc-inline-list__item"))
	   for img = (dom-attr (dom-by-tag elem 'img) 'src)
	   for title = (dom-texts (dom-by-class elem "ipc-title-link-wrapper"))
	   while (< i (or max-results 10))
	   collect (format
		    "%s %s, %s, %s"
		    (and img (with-current-buffer
				 (imdb-url-retrieve-synchronously img)
			       (url-store-in-cache)
			       (goto-char (point-min))
			       (when (search-forward "\n\n" nil t)
				 (propertize
				  "*"
				  'display
				  (create-image
				   (buffer-substring (point) (point-max))
				   nil t)))))
		    (string-replace "," "" title)
		    year
		    id)))

(defun imdb-query (title &optional max-results)
  "Query IMDB for TITLE, and then prompt the user for the right match."
  (interactive "sTitle: ")
  (let* ((max-mini-window-height 0.5)
	 (data (append
		(imdb-extract-data (imdb-get-data title) max-results)
		(and (file-exists-p "~/.emacs.d/imdb/imdb.sqlite3")
		     (cl-loop for film in (imdb-matching-films title)
			      collect (format " %s, %s, %s, %s"
					      (string-replace
					       "," ""
					       (plist-get film :title))
					      (plist-get film :year)
					      (plist-get film :id)
					      (string-replace
					       ", " " + "
					      (plist-get film :director)))))))
	 (result (if (and max-results (= max-results 1))
		     (car data)
		   (if data
		       (completing-read "Movie: " (cdr data) nil nil
					(cons (car data) 0))
		     (completing-read "Movie: " nil)))))
    (when (and data
	       (string-match " *\\([^,]+\\), *\\([^,]+\\), *\\([^,]+\\)"
			     result))
      (let ((res
	     (list :year (match-string 2 result)
		   :id (match-string 3 result)))
	    (more (imdb-get-image-and-country (match-string 3 result))))
	(append res
		(list :country (nth 1 more)
		      :director (nth 2 more)
		      :image (nth 0 more)))))))

(provide 'imdb)

;;; imdb.el ends here
