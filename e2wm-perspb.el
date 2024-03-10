;;; e2wm-perspb.el --- Plugin of e2wm.el for buffer list of persp-mode.el

;; Copyright (C) 2017  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: tools, window manager
;; URL: https://github.com/aki2o/e2wm-perspb
;; Version: 0.0.9
;; Package-Requires: ((e2wm "1.2") (persp-mode "2.9.4") (dash "2.12.0") (deferred "0.3.1") (yaxception "1.0.0") (all-the-icons "3.2.0"))

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

;;; Commentary:

;; (setq e2wm:c-code-recipe
;;       '(| (:left-max-size 40)
;;           (- (:upper-size-ratio 0.6)
;;              files history)
;;           (- (:lower-max-size 150)
;;              (| (:right-max-size 40)
;;                 main imenu)
;;              sub)))
;;
;; (setq e2wm:c-code-winfo
;;       '((:name main)
;;         (:name files   :plugin files)
;;         (:name history :plugin perspb)
;;         (:name imenu   :plugin imenu :default-hide nil)
;;         (:name sub     :buffer "*info*" :default-hide t)))
;;
;; (e2wm:add-keymap
;;  e2wm:dp-two-minor-mode-map
;;  '(("C-." . e2wm-perspb:switch-to-down-entry-command)
;;    ("C-," . e2wm-perspb:switch-to-up-entry-command)
;;    ) e2wm:prefix-key)
;;
;; For detail, see <https://github.com/aki2o/e2wm-perspb/blob/master/README.md>
;;
;; Enjoy!!!

;;; Code:

(require 'cl-lib)
(require 'e2wm)
(require 'persp-mode)
(require 'dash)
(require 'concurrent)
(require 'yaxception)
(require 'all-the-icons)


(defgroup e2wm-perspb nil
  "Plugin of e2wm.el for persp-mode."
  :group 'windows
  :prefix "e2wm-perspb:")

(defcustom e2wm-perspb:entry-makers '(e2wm-perspb:make-normal-entry)
  "List of function to make entry in e2wm-perspb:mode buffer."
  :type (list 'function)
  :group 'e2wm-perspb)

(defcustom e2wm-perspb:inhibit-add-buffer-commands '(e2wm:dp-two-swap-buffers-command)
  "List of command to inhibit `persp-add-buffer'."
  :type (list 'command)
  :group 'e2wm-perspb)

(defcustom e2wm-perspb:sort-function 'e2wm-perspb:sort-entries
  "Function to sort entries in e2wm-perspb:mode buffer."
  :type 'function
  :group 'e2wm-perspb)

(defface e2wm-perspb:file-buffer-face
  '((t (:foreground "ivory")))
  "Face for file buffer."
  :group 'e2wm-perspb)

(defface e2wm-perspb:non-file-buffer-face
  '((t (:foreground "gray")))
  "Face for non-file buffer."
  :group 'e2wm-perspb)

(defface e2wm-perspb:current-highlight-face
  '((t (:background "gray30")))
  "Face for active entry."
  :group 'e2wm-perspb)


(defun e2wm-perspb:make-normal-entry (buf)
  `(:name ,(buffer-name buf)
          :mark nil
          :name-face ,(if (buffer-file-name buf)
                          'e2wm-perspb:file-buffer-face
                        'e2wm-perspb:non-file-buffer-face)
          :mark-face nil))

(defun e2wm-perspb:sort-entries (entries)
  (let ((marks '()))
    (cl-loop for e in entries
             for key = (plist-get e :mark)
             for alist = (assoc key marks)
             do (if alist
                    (setf (cdr alist) (append (cdr alist) (list e)))
                  (push (cons key (list e)) marks)))
    (cl-loop for alist in (sort marks (lambda (a b)
                                        (let ((abufname (buffer-file-name (plist-get (cadr a) :buffer)))
                                              (bbufname (buffer-file-name (plist-get (cadr b) :buffer))))
                                          (cond ((and abufname bbufname) (string< (car a) (car b)))
                                                (abufname                t)
                                                (t                       nil)))))
             append (sort (cdr alist) (lambda (a b)
                                        (string< (plist-get a :name) (plist-get b :name)))))))

(defvar e2wm-perspb::buffer-name " *WM:Perspb*")

(e2wm:plugin-register 'perspb "Perspb" 'e2wm-perspb:def-plugin)

(defun e2wm-perspb:def-plugin (frame wm winfo)
  (let* ((wname (wlf:window-name winfo))
         (buf (e2wm-perspb::ensure-buffer))
         (buf-list (-filter 'e2wm:history-recordable-p (cl-delete-if
                                                        'persp-buffer-filtered-out-p
                                                        (persp-buffer-list-restricted))))
         (entries (cl-loop for b in buf-list
                           for entry = (cl-loop for f in e2wm-perspb:entry-makers
                                                for entry = (funcall f b)
                                                if entry return entry
                                                finally return (e2wm-perspb:make-normal-entry b))
                           collect (e2wm-perspb::setup-entry entry b)))
         focused-buf)
    (with-current-buffer buf
      (yaxception:$
        (yaxception:try
          (setq focused-buf (get-text-property (pos-bol) 'e2wm:buffer))
          (setq buffer-read-only nil)
          (erase-buffer)
          (cl-loop initially (goto-char (point-min))
                   with nextpt = nil
                   for entry in (if e2wm-perspb:sort-function
                                    (funcall e2wm-perspb:sort-function entries)
                                  entries)
                   for pt = (point)
                   for line = (format "%s %s"
                                      (e2wm:rt (plist-get entry :mark) (plist-get entry :mark-face))
                                      (e2wm:rt (plist-get entry :name) (plist-get entry :name-face)))
                   do (insert
                       (if (> pt (point-min)) "\n" "")
                       (e2wm:tp line 'e2wm:buffer (plist-get entry :buffer)))
                   if (eql focused-buf (plist-get entry :buffer))
                   do (setq nextpt (pos-bol))
                   finally do (set-window-point (get-buffer-window) (goto-char (or nextpt (point-min)))))
          (e2wm-perspb::update-current-highlight)
          (setq mode-line-format
                '("-" mode-line-mule-info " " mode-line-position "-%-"))
          (setq header-line-format
                (format "%s [%i]"
                        (or (e2wm:aif (get-current-persp)
                                (persp-name it))
                            "none")
                        (length entries))))
        (yaxception:finally
          (setq buffer-read-only t))))
    (wlf:set-buffer wm wname buf)))

(defun e2wm-perspb::ensure-buffer ()
  (let ((buf (get-buffer e2wm-perspb::buffer-name)))
    (if (and buf (buffer-live-p buf))
        buf
      (with-current-buffer (get-buffer-create e2wm-perspb::buffer-name)
        (e2wm-perspb:mode)
        (setq buffer-read-only t)
        (setq truncate-lines t)
        (buffer-disable-undo buf)
        (current-buffer)))))

(defun e2wm-perspb::setup-entry (entry buf)
  (let ((all-the-icons-scale-factor 1.0)
        (all-the-icons-default-adjust 0))
    `(:buffer ,buf
              :name ,(plist-get entry :name)
              :mark ,(or (plist-get entry :mark)
                         (e2wm:aif (buffer-file-name buf) (all-the-icons-icon-for-file it))
                         (let* ((mode (buffer-local-value 'major-mode buf))
                                (icon (all-the-icons-icon-for-mode mode)))
                           (if (eq icon mode)
                               (all-the-icons-icon-for-mode 'fundamental-mode)
                             icon)))
              :name-face ,(if (buffer-modified-p buf) 'font-lock-warning-face (plist-get entry :name-face))
              :mark-face ,(if (buffer-modified-p buf) 'font-lock-warning-face (plist-get entry :mark-face)))))

(defvar e2wm-perspb::current-highlight nil)

(defun e2wm-perspb::update-current-highlight ()
  (when (not e2wm-perspb::current-highlight)
    (set (make-local-variable 'e2wm-perspb::current-highlight)
         (e2wm-perspb::make-current-highlight)))
  (move-overlay e2wm-perspb::current-highlight (pos-bol) (1+ (pos-eol))))

(defun e2wm-perspb::make-current-highlight ()
  (let ((ov (make-overlay (point) (point))))
    (overlay-put ov 'priority -50)
    (overlay-put ov 'face 'e2wm-perspb:current-highlight-face)
    ov))

(defvar e2wm-perspb:mode-map
  (e2wm:define-keymap
   '(
     ("p" . previous-line)
     ("n" . next-line)
     ("k" . previous-line)
     ("j" . next-line)
     
     ("C-m" . e2wm-perspb:select-command)
     ("q"   . e2wm:pst-window-select-main-command)
     )))

(define-derived-mode e2wm-perspb:mode fundamental-mode "Perspb")


;; User Command

(defun e2wm-perspb:select-command ()
  (interactive)
  (when (e2wm:managed-p)
    (let ((buf (get-text-property (pos-bol) 'e2wm:buffer)))
      (switch-to-buffer buf)
      (e2wm:pst-window-select-main))))

(defun e2wm-perspb:switch-to-up-entry-command ()
  (interactive)
  (when (e2wm:managed-p)
    (let ((wm (e2wm:pst-get-wm))
          (wnd (get-buffer-window e2wm-perspb::buffer-name)))
      (when wnd
        (with-selected-window wnd
          (forward-line 1)
          (beginning-of-line)
          (e2wm-perspb:select-command))))))

(defun e2wm-perspb:switch-to-down-entry-command ()
  (interactive)
  (when (e2wm:managed-p)
    (let ((wm (e2wm:pst-get-wm))
          (wnd (get-buffer-window e2wm-perspb::buffer-name)))
      (when wnd
        (with-selected-window wnd
          (forward-line -1)
          (beginning-of-line)
          (e2wm-perspb:select-command))))))


;; Auto refresh

(defvar e2wm-perspb::refresh-semaphore (cc:semaphore-create 1))

(defun e2wm-perspb:set-refresh-timer (frame-or-window)
  (when (> (cc:semaphore-permits e2wm-perspb::refresh-semaphore) 0)
    (cc:semaphore-acquire e2wm-perspb::refresh-semaphore)
    (run-with-idle-timer
     idle-update-delay
     nil
     #'(lambda ()
         (yaxception:$
           (yaxception:try
             (when (e2wm:managed-p)
               (e2wm:plugin-exec-update-by-plugin-name
                (selected-frame) (e2wm:pst-get-wm) 'perspb)))
           (yaxception:finally
             (cc:semaphore-release e2wm-perspb::refresh-semaphore)))))))

(defadvice persp-add-buffer (after e2wm-perspb:refresh activate)
  (e2wm-perspb:set-refresh-timer 'frame))

(add-to-list 'persp-activated-functions 'e2wm-perspb:set-refresh-timer t)


;; Auto add buffer

(defvar e2wm-perspb::add-buffer-enabled t)

(defun e2wm-perspb:add-buffer (buf)
  (when (and persp-mode
             e2wm-perspb::add-buffer-enabled
             (e2wm:managed-p)
             (not (memq this-command e2wm-perspb:inhibit-add-buffer-commands))
             (or (e2wm:document-buffer-p buf)
                 (e2wm:history-recordable-p buf)))
    (let ((persp-switch-to-added-buffer nil))
      (persp-add-buffer buf))))

(defadvice persp-activate (around e2wm-perspb:disable-add-buffer activate)
  (let ((e2wm-perspb::add-buffer-enabled nil))
    ad-do-it))

(defadvice display-buffer (after e2wm-perspb:add-buffer activate)
  (e2wm-perspb:add-buffer (ad-get-arg 0)))

(defadvice set-window-buffer (after e2wm-perspb:add-buffer activate)
  (e2wm-perspb:add-buffer (ad-get-arg 0)))

(defadvice wlf:set-buffer (after e2wm-perspb:add-buffer activate)
  (e2wm-perspb:add-buffer (ad-get-arg 2)))


(provide 'e2wm-perspb)
;;; e2wm-perspb.el ends here
