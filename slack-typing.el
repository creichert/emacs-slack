;;; slack-typing.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <yuya373@archlinux>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eieio)
(require 'slack-team)
(require 'slack-buffer)

(defclass slack-typing ()
  ((room :initarg :room :initform nil)
   (limit :initarg :limit :initform nil)
   (users :initarg :users :initform nil)))

(defclass slack-typing-user ()
  ((limit :initarg :limit :initform nil)
   (user-name :initarg :user-name :initform nil)))

(defun slack-typing-cancel-timer (team)
  (with-slots (typing-timer) team
    (when (timerp typing-timer)
      (cancel-timer typing-timer)
      (setq typing-timer nil))))

(defun slack-typing-dispatch-timer (team)
  (oset team
        typing-timer
        (run-with-timer t 1 #'slack-user-typing team)))

(defun slack-user-typing (team)
  (with-slots (typing) team
    (let ((current (float-time)))
      (if (or (null typing)
              (< (oref typing limit) current))
          (progn
            (slack-typing-cancel-timer team)
            (setq typing nil)
            (message ""))
        (slack-if-let* ((typing (oref team typing))
                        (room (oref typing room))
                        (buf (slack-buffer-find 'slack-message-buffer room team))
                        (show-typing-p (slack-buffer-show-typing-p
                                        (get-buffer (slack-buffer-name buf)))))
            (let ((visible-users (cl-remove-if
                                  #'(lambda (u) (< (oref u limit) current))
                                  (oref typing users))))
              (slack-log
               (format "%s is typing..."
                       (mapconcat #'(lambda (u) (oref u user-name))
                                  visible-users
                                  ", "))
               team
               :level 'info)))))))

(defmethod slack-equalp ((this slack-typing) other)
  (string= (oref (oref this room) id)
           (oref (oref other room) id)))

(provide 'slack-typing)
;;; slack-typing.el ends here
