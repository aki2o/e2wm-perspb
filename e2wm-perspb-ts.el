;;; e2wm-perspb-ts.el --- font-lock for e2wm-perspb.el in Typescript/Javascript project

;; Copyright (C) 2024  Hiroaki Otsu

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'e2wm-perspb)


(defgroup e2wm-perspb-ts nil
  "Feature of e2wm-perspb for Typescript/Javascript buffer."
  :group 'e2wm-perspb
  :prefix "e2wm-perspb-ts:")

(defcustom e2wm-perspb-ts:modes '(typescript-mode js2-mode rjsx-mode)
  "Major modes to activate e2wm-perspb-ts."
  :type (list 'symbol)
  :group 'e2wm-perspb-ts)

(defface e2wm-perspb-ts:router-mark-face
  '((t (:background "lime green" :foreground "white" :bold t)))
  "Face for makr of router."
  :group 'e2wm-perspb-ts)

(defface e2wm-perspb-ts:router-name-face
  '((t (:foreground "lime green")))
  "Face for name of router."
  :group 'e2wm-perspb-ts)

(defface e2wm-perspb-ts:declare-mark-face
  '((t (:background "dodger blue" :foreground "white" :bold t)))
  "Face for mark of declare."
  :group 'e2wm-perspb-ts)

(defface e2wm-perspb-ts:declare-name-face
  '((t (:foreground "dodger blue")))
  "Face for name of declare."
  :group 'e2wm-perspb-ts)

(defface e2wm-perspb-ts:tsx-mark-face
  '((t (:background "sandy brown" :foreground "white" :bold t)))
  "Face for mark of tsx."
  :group 'e2wm-perspb-ts)

(defface e2wm-perspb-ts:tsx-name-face
  '((t (:foreground "sandy brown")))
  "Face for name of tsx."
  :group 'e2wm-perspb-ts)

(defface e2wm-perspb-ts:factory-mark-face
  '((t (:background "medium purple" :foreground "white" :bold t)))
  "Face for mark of factory."
  :group 'e2wm-perspb-ts)

(defface e2wm-perspb-ts:factory-name-face
  '((t (:foreground "medium purple")))
  "Face for name of factory."
  :group 'e2wm-perspb-ts)

(defface e2wm-perspb-ts:test-mark-face
  '((t (:background "violet red" :foreground "white" :bold t)))
  "Face for mark of test."
  :group 'e2wm-perspb-ts)

(defface e2wm-perspb-ts:test-name-face
  '((t (:foreground "violet red")))
  "Face for name of test."
  :group 'e2wm-perspb-ts)


(defun e2wm-perspb-ts:mode-p (mode)
  (cl-loop while mode
           if (memq mode e2wm-perspb-ts:modes)
           return t
           else do (setq mode (get mode 'derived-mode-parent))))

(defun e2wm-perspb-ts:make-entry (buf)
  (when (e2wm-perspb-ts:mode-p (buffer-local-value 'major-mode buf))
    (let ((filepath (expand-file-name (buffer-file-name buf)))
          (bufname (buffer-name buf))
          (router-re (rx "/pages/"))
          (declare-re (rx ".d.ts" eos))
          (tsx-re (rx (or ".jsx" ".tsx") eos))
          (factory-re (rx "/factories/"))
          (test-re (rx (or "/test/" "/__test__/" ".test."))))
      (cond
       ((string-match router-re filepath)
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-ts:router-name-face
                :mark-face e2wm-perspb-ts:router-mark-face))
       ((string-match declare-re filepath)
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-ts:declare-name-face
                :mark-face e2wm-perspb-ts:declare-mark-face))
       ((string-match tsx-re filepath)
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-ts:tsx-name-face
                :mark-face e2wm-perspb-ts:tsx-mark-face))
       ((string-match factory-re filepath)
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-ts:factory-name-face
                :mark-face e2wm-perspb-ts:factory-mark-face))
       ((string-match test-re filepath)
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-ts:test-name-face
                :mark-face e2wm-perspb-ts:test-mark-face))))))

(add-to-list 'e2wm-perspb:entry-makers 'e2wm-perspb-ts:make-entry)


(provide 'e2wm-perspb-ts)
;;; e2wm-perspb-ts.el ends here
