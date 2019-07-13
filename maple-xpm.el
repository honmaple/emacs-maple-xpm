;;; maple-xpm.el --- create xpm image configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-modeline

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; image generating configurations.
;;

;;; Code:
(require 'color)

(defgroup maple-xpm nil
  "Create xpm image."
  :group 'maple)

(defcustom maple-xpm-style 'default
  "Xpm image style."
  :group 'maple-xpm
  :type '(choice (const default)
                 (const wave)
                 (const line)
                 (const bar)
                 (const slant)
                 (const contour)
                 (const box)
                 (const butt)
                 (const curve)
                 (const zigzag)
                 (const gradient)))

(defcustom maple-xpm-height (- (elt (window-pixel-edges) 3)
                               (elt (window-inside-pixel-edges) 3))
  "Height of the mode line in pixels.
This should be an even number."
  :type 'integer
  :group 'maple-xpm)

(defvar maple-xpm-cache nil)
(defvar maple-xpm-chars
  (append (mapcar 'number-to-string (number-sequence 0 9))
          (mapcar 'char-to-string (number-sequence ?a ?z))))

(defun maple-xpm--background (face)
  "Get FACE background."
  (let ((background (if (listp face) (plist-get face :background )
                      (face-attribute face :background nil t))))
    (if (or (not background)
            (eq background 'unspecified))
        (face-attribute 'default :background)
      background)))

(defun maple-xpm--height ()
  "Get default height."
  (or maple-xpm-height (frame-char-height)))

(defun maple-xpm--string(pattern)
  "To string with PATTERN."
  (concat "\"" (mapconcat (lambda(x) (format "%s" x)) pattern "") "\","))

(defun maple-xpm--reverse(pattern)
  "PATTERN."
  (mapcar 'reverse pattern))

(defun maple-xpm-reset(&rest _)
  "Reset xpm cache."
  (setq maple-xpm-cache nil))

(defun maple-xpm-draw(face1 face2 &optional reverse height width style)
  "Draw FACE1 FACE2 &OPTIONAL REVERSE HEIGHT WIDTH STYLE."
  (let* ((style (or style maple-xpm-style))
         (func (intern (format "maple-xpm-%s" style)))
         (key (list style face1 face2 height reverse)))
    (or (cdr (assoc key maple-xpm-cache))
        (let ((image (propertize
                      " " 'display (funcall func face1 face2 reverse height width))))
          (push (cons key image) maple-xpm-cache) image))))

(defmacro maple-xpm-define (name center &optional header footer)
  "NAME CENTER &OPTIONAL HEADER FOOTER."
  (declare (indent 1)
           (doc-string 2))
  (let* ((-name (format "%s" name)))
    `(defun ,(intern (format "maple-xpm-%s" -name)) (face1 face2 &optional reverse height width)
       (when window-system
         (when reverse (setq face1 (prog1 face2 (setq face2 face1))))
         (let* ((name (replace-regexp-in-string "-" "_" ,-name))
                (color1 (or (maple-xpm--background face1) "None"))
                (color2 (or (maple-xpm--background face2) "None"))
                (color3 color1)
                (height (or height (maple-xpm--height)))
                (width (or width (length (or (car ,center) (car ,header) (car ,footer)))))
                (dir (if reverse "right" "left"))
                (header-pattern (mapcar 'maple-xpm--string
                                        (if reverse (maple-xpm--reverse ,header) ,header)))
                (footer-pattern (mapcar 'maple-xpm--string
                                        (if reverse (maple-xpm--reverse ,footer) ,footer)))
                (pattern (mapcar 'maple-xpm--string
                                 (if reverse (maple-xpm--reverse ,center) ,center)))
                (pattern-height (max (- height (+ (length ,header) (length ,footer))) 0)))
           (create-image
            (format "/* XPM */ static char * %s_%s[] = {
                                         \"%s %s 3 1\",
                                         \"0 c %s\",
                                         \"1 c %s\",
                                         \"2 c %s\",
                                         %s};"
                    name dir width height color1 color2 color3
                    (concat (when ,header
                              (mapconcat 'identity header-pattern ""))
                            (when ,center
                              (mapconcat 'identity (make-list pattern-height (mapconcat 'identity pattern "")) ""))
                            (when ,footer
                              (mapconcat 'identity footer-pattern ""))))
            'xpm t
            :ascent 'center))))))


(maple-xpm-define line
  '((2)))

(maple-xpm-define bar
  '((2 2)))

(maple-xpm-define wave
  '((0 0 0 0 0 0 1 1 1 1 1))
  '((2 1 1 1 1 1 1 1 1 1 1)
    (0 0 1 1 1 1 1 1 1 1 1)
    (0 0 0 1 1 1 1 1 1 1 1)
    (0 0 0 2 1 1 1 1 1 1 1)
    (0 0 0 0 1 1 1 1 1 1 1)
    (0 0 0 0 2 1 1 1 1 1 1)
    (0 0 0 0 0 1 1 1 1 1 1)
    (0 0 0 0 0 1 1 1 1 1 1)
    (0 0 0 0 0 2 1 1 1 1 1))
  '((0 0 0 0 0 0 2 1 1 1 1)
    (0 0 0 0 0 0 0 1 1 1 1)
    (0 0 0 0 0 0 0 1 1 1 1)
    (0 0 0 0 0 0 0 2 1 1 1)
    (0 0 0 0 0 0 0 0 1 1 1)
    (0 0 0 0 0 0 0 0 2 1 1)
    (0 0 0 0 0 0 0 0 0 0 2)))

(maple-xpm-define contour
  '((0 0 0 0 0 1 1 1 1 1))
  '((1 1 1 1 1 1 1 1 1 1)
    (0 2 1 1 1 1 1 1 1 1)
    (0 0 2 1 1 1 1 1 1 1)
    (0 0 0 2 1 1 1 1 1 1)
    (0 0 0 0 1 1 1 1 1 1)
    (0 0 0 0 2 1 1 1 1 1))
  '((0 0 0 0 0 2 1 1 1 1)
    (0 0 0 0 0 0 1 1 1 1)
    (0 0 0 0 0 0 2 1 1 1)
    (0 0 0 0 0 0 0 2 1 1)
    (0 0 0 0 0 0 0 0 0 0)))

(maple-xpm-define butt
  '((0 0 0))
  '((1 1 1)
    (0 1 1)
    (0 0 1))
  '((0 0 1)
    (0 1 1)
    (1 1 1)))

(maple-xpm-define box
  '((0 0)
    (0 0)
    (1 1)
    (1 1)))

(maple-xpm-define curve
  '((0 0 0 0))
  '((1 1 1 1)
    (2 1 1 1)
    (0 0 1 1)
    (0 0 2 1)
    (0 0 0 1)
    (0 0 0 2))
  '((0 0 0 2)
    (0 0 0 1)
    (0 0 2 1)
    (0 0 1 1)
    (2 1 1 1)
    (1 1 1 1)))

(maple-xpm-define zigzag
  '((1 1 1)
    (0 1 1)
    (0 0 1)
    (0 0 0)
    (0 0 1)
    (0 1 1)))

(maple-xpm-define slant
  nil
  (cl-loop
   for i from 1 to height collect
   (let ((x (/ i 2)))
     (append (make-list x 0)
             (make-list 1 2)
             (make-list (max 0 (- 10 x)) 1)))))

(defun maple-xpm-gradient (face1 face2 &optional reverse height width)
  "FACE1 FACE2 &OPTIONAL REVERSE HEIGHT WIDTH."
  (ignore reverse)
  (let* ((color1 (maple-xpm--background face1))
         (color2 (maple-xpm--background face2))
         (height (or height (maple-xpm--height)))
         (width  (or width 13))
         (number -1))
    (create-image
     (format "/* XPM */ static char * gradient[] = {\"%s %s %s 1\", %s %s};"
             width height width
             (mapconcat
              (lambda(x)
                (setq number (+ number 1))
                (format "\"%s c %s\"," (nth number maple-xpm-chars) (apply 'color-rgb-to-hex x)))
              (color-gradient
               (color-name-to-rgb color1)
               (color-name-to-rgb color2) width) "")
             (mapconcat
              'identity
              (make-list height (maple-xpm--string
                                 (subseq maple-xpm-chars 0 (min width (length maple-xpm-chars))))) ""))
     'xpm t
     :ascent 'center)))

(defun maple-xpm-default (face1 face2 &optional reverse height width)
  "FACE1 FACE2 &OPTIONAL REVERSE HEIGHT WIDTH."
  (ignore height) (ignore width)
  (propertize
   (char-to-string (if reverse #xe0b2 #xe0b0))
   'face (list :background (maple-xpm--background (if reverse face1 face2))
               :foreground (maple-xpm--background (if reverse face2 face1)))))

(advice-add 'load-theme :after #'maple-xpm-reset)

(provide 'maple-xpm)
;;; maple-xpm.el ends here
