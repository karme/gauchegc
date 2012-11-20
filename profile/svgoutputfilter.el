;;;; svgoutputfilter.el --- SVG (pre-)output filter for comint-mode

;; Copyright (C) 2012 Jens Thiele

;; Author: Jens Thiele <karme@karme.de>
;; Created: 20 Nov 2012
;; Version: 1.0
;; Keywords: SVG image output filter graphical shell

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;

;;; Commentary:
;;
;; SVG image support for comint-mode buffers inspired by imaxima.el
;;
;; To use run M-x add-svg-output-filter in a comint-mode buffer
;; any output like: <svg>...</svg> will be replaced with an image
;; To turn off again run M-x remove-svg-output-filter
;;
;; example run:
;; M-x shell
;; M-x add-svg-output-filter
;; $ echo -e '[a] -> [b]\n[a] -> [c]\n[b]->[d]->[c]'|graph-easy --as dot|dot -Tsvg
;;
;; todos:
;; - xml start is treated like svg start
;;   (would need one more state to fix that)
;; - someone with more elisp knowledge should take a look at it
;;
;; notes:
;; - to make a compilation buffer a comint buffer run M-x compile with
;;   prefix arg!

(defvar svg-output-filter-buf '() "buffer to collect svg")
;;(defvar svg-start "<svg")
(defvar svg-start "\\\(<svg\\\)\\\|\\\(<\\\?xml\\\)")

(defun svg-output-filter-svg-end (s)
  "process end of svg: insert image and empty svg-output-filter-buf buffer"
  ;;(message "found end of svg")
  (let ((epos (+ (string-match "</svg>" s) (length "</svg>"))))
    (setq svg-output-filter-buf (cons (substring s 0 epos) svg-output-filter-buf))
    (insert-image (create-image (apply 'concat (reverse svg-output-filter-buf)) 'svg 1))
    (set-marker (process-mark (get-buffer-process (current-buffer))) (point)) ;; todo: hmm
    (setq svg-output-filter-buf nil)
    (substring s epos)))

(defun svg-output-filter (s)
  "(pre-)output filter replacing svg with image"
  (cond ((and (not svg-output-filter-buf)
	      (string-match svg-start s))
	 (let ((spos (string-match svg-start s)))
	   ;;(message "found start of svg")
	   (let ((buf (substring s spos)))
	     (cond ((string-match "</svg>" buf)
		    (concat (substring s 0 spos)
			    (svg-output-filter-svg-end buf)))
		   (t
		    (setq svg-output-filter-buf (list buf))
		    (substring s 0 spos))))))
	((and svg-output-filter-buf
	      (string-match "</svg>" s))
	 (svg-output-filter-svg-end s))
	(svg-output-filter-buf
	 (setq svg-output-filter-buf (cons s svg-output-filter-buf))
	 "")
	(t
	 s)))
  
(defun add-svg-output-filter ()
  "Add svg output filter to current comint-mode buffer"
  (interactive)
  (make-local-variable 'svg-output-filter-buf)
  (make-local-variable 'svg-start)
  (add-hook 'comint-preoutput-filter-functions
	    'svg-output-filter
	    t
	    t))

(defun remove-svg-output-filter ()
  "Remove svg output filter from current comint-mode buffer"
  (interactive)
  (remove-hook 'comint-preoutput-filter-functions
	       'svg-output-filter
	       t))

(provide 'svgoutputfilter)
