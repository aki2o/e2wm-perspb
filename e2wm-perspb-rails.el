;;; e2wm-perspb-rails.el --- font-lock for e2wm-perspb.el in rails project

;; Copyright (C) 2017  Hiroaki Otsu

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


(defgroup e2wm-perspb-rails nil
  "Feature of e2wm-perspb for rails buffer."
  :group 'e2wm-perspb
  :prefix "e2wm-perspb-rails:")

(defcustom e2wm-perspb-rails:modes '(ruby-mode slim-mode haml-mode)
  "Major modes to activate e2wm-perspb-rails."
  :type (list 'symbol)
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:controller-mark-face
  '((t (:background "lime green" :foreground "white" :bold t)))
  "Face for makr of controller."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:controller-name-face
  '((t (:foreground "lime green")))
  "Face for name of controller."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:model-mark-face
  '((t (:background "dodger blue" :foreground "white" :bold t)))
  "Face for mark of model."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:model-name-face
  '((t (:foreground "dodger blue")))
  "Face for name of model."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:view-mark-face
  '((t (:background "sandy brown" :foreground "white" :bold t)))
  "Face for mark of view."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:view-name-face
  '((t (:foreground "sandy brown")))
  "Face for name of view."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:other-app-mark-face
  '((t (:background "beige" :foreground "dark slate blue" :bold t)))
  "Face for mark of other under app."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:other-app-name-face
  '((t (:foreground "beige")))
  "Face for name of other under app."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:factory-mark-face
  '((t (:background "medium purple" :foreground "white" :bold t)))
  "Face for mark of factory."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:factory-name-face
  '((t (:foreground "medium purple")))
  "Face for name of factory."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:test-mark-face
  '((t (:background "violet red" :foreground "white" :bold t)))
  "Face for mark of test."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:test-name-face
  '((t (:foreground "violet red")))
  "Face for name of test."
  :group 'e2wm-perspb-rails)


(defun e2wm-perspb-rails:mode-p (mode)
  (cl-loop while mode
           if (memq mode e2wm-perspb-rails:modes)
           return t
           else do (setq mode (get mode 'derived-mode-parent))))

(defun e2wm-perspb-rails:make-entry (buf)
  (when (e2wm-perspb-rails:mode-p (buffer-local-value 'major-mode buf))
    (let ((filepath (expand-file-name (buffer-file-name buf)))
          (bufname (buffer-name buf)))
      (cond
       ((string-match "app/controllers/.+_controller\\.rb\\'" filepath)
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-rails:controller-name-face
                :mark-face e2wm-perspb-rails:controller-mark-face))
       ((string-match "app/models/[^/]+\\.rb\\'" filepath)
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-rails:model-name-face
                :mark-face e2wm-perspb-rails:model-mark-face))
       ((string-match "app/views/.+\\.html\\." filepath)
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-rails:view-name-face
                :mark-face e2wm-perspb-rails:view-mark-face))
       ((string-match "app/.+\\.rb\\'" filepath)
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-rails:other-app-name-face
                :mark-face e2wm-perspb-rails:other-app-mark-face))
       ((string-match "factories/.+\\.rb\\'" filepath)
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-rails:factory-name-face
                :mark-face e2wm-perspb-rails:factory-mark-face))
       ((or (string-match "spec/.+_spec\\.rb\\'" filepath)
            (string-match "test/.+_test\\.rb\\'" filepath))
        `(:name ,bufname
                :mark nil
                :name-face e2wm-perspb-rails:test-name-face
                :mark-face e2wm-perspb-rails:test-mark-face))))))

(add-to-list 'e2wm-perspb:entry-makers 'e2wm-perspb-rails:make-entry)


(provide 'e2wm-perspb-rails)
;;; e2wm-perspb-rails.el ends here
