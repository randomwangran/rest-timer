j;;; rest-timer.el --- Simple countdown timer      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Daniel Kraus
;;
;; Author: Daniel Kraus <daniel@kraus.my>
;;         Ran Wang
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, timer
;; URL: https://github.com/randomwangran/rest-timer

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
;; This package is used from an Org-mode entry starting with `org-clock-in`.
;;
;; The point is to intentionally ask the user the expcted time
;; duration for a task. If there is no expcted time, it will prompt a choice.
;; Otherwise, it will start to clock in.
;;
;; A hard stop will be executed, if the effort is over a certain
;; duration. For example, 49 minutes. Once the time is over, the
;; buffer will switch to a special buffer to remind the user.
;;
;; At the end of day, be-aware of that:
;;
;;                              知足常乐，贪则多忧。
;;                                                   《道德经》

;;; Code:
(require 'org)

(add-hook 'org-clock-in-prepare-hook
          'rest-timer-org-mode-ask-effort)

(defvar my-org-clock-default-effort '("5" "20" "50:00"))

(defgroup rest-timer nil
  "rest-timer"
  :prefix "rest-timer-"
  :group 'convenience)

(defcustom rest-timer-default-duration 50
  "Default timer duration."
  :type 'integer
  :safe #'integerp
  :group 'rest-timer)

(defcustom rest-timer-threshold 49
  "Time for running rest timer."
  :type 'integer
  :safe #'integerp
  :group 'rest-timer)

(defcustom rest-timer-message "为无为，事无事，味无味。"
  "Message to show when timer is up."
  :type 'string
  :group 'rest-timer)

(defvar rest-timer--timer nil
  "Store current running timer.")

(defun rest-butterfly ()
  "Use butterflies to flip the desired bit on the drive platter.
Open hands and let the delicate wings flap once.  The disturbance
ripples outward, changing the flow of the eddy currents in the
upper atmosphere.  These cause momentary pockets of higher-pressure
air to form, which act as lenses that deflect incoming cosmic rays,
focusing them to strike the drive platter and flip the desired bit.
You can type `M-x butterfly C-M-c' to run it.  This is a permuted
variation of `C-x M-c M-butterfly' from url `http://xkcd.com/378/'.

Yin and Yang by Normand Veilleux.
<https://www.asciiart.eu/religion/yin-and-yang>"
  (interactive)
  (if t
      (progn
	(switch-to-buffer (get-buffer-create "*butterfly*"))
	(erase-buffer)
	(sit-for 0)
	(animate-string "
                                                                .,ad88888888baa,
                                                            ,d8P\"\"\"        \"\"9888ba.
                                                         .a8\"          ,ad88888888888a
                                                        aP'          ,88888888888888888a
                                                      ,8\"           ,88888888888888888888,
                                                     ,8'            (888888888( )888888888,
                                                    ,8'             `8888888888888888888888
                                                    8)               `888888888888888888888,
                                                    8                  \"8888888888888888888)
                                                    8                   `888888888888888888)
                                                    8)                    \"8888888888888888
                                                    (b                     \"88888888888888'
                                                    `8,        (8)          8888888888888)
                                                     \"8a                   ,888888888888)
                                                       V8,                 d88888888888\"
                                                        `8b,             ,d8888888888P'
                                                          `V8a,       ,ad8888888888P'
                                                             \"\"88888888888888888P\"
                                                                  \"\"\"\"\"\"\"\"\"\"\"\"


                                                                             知足常乐，贪则多忧。"
			(/ (window-height) 4) (/ (window-width) 2))
        (read-only-mode)
	(sit-for (* 5 (/ (abs (random)) (float most-positive-fixnum))))
	(message "知足常乐，贪则多忧。"))))

(defun rest-timer-rest-ready ()
  "Display rest ready message and reset timer."
  (if (require 'alert nil 'no-error)
      (alert rest-timer-message)
    (rest-butterfly))
  (setq rest-timer--timer nil))

;;;###autoload
(defun rest-timer (&optional duration)
  "Set a rest timer to DURATION in seconds or rest-timer-default-duration."
  (interactive "P")
  (when rest-timer--timer
    (when (y-or-n-p "Another rest timer already running.  Cancel the old one? ")
      (rest-timer-cancel)
      (setq rest-timer--timer nil)))
  (if rest-timer--timer
      (rest-timer-display-remaining-time)
    (message "Setting rest timer to %s minutes." (or duration rest-timer-default-duration))
    (setq rest-timer--timer
          (run-at-time (* 60 (or duration rest-timer-default-duration)) nil 'rest-timer-rest-ready))))

(defun rest-timer-display-remaining-time ()
  "Displays remaining time in the Minibuffer."
  (interactive)
  (if rest-timer--timer
      (let* ((remaining-time (decode-time (time-subtract (timer--time rest-timer--timer) (current-time)) t))
             (minutes (nth 1 remaining-time))
             (seconds (nth 0 remaining-time)))
        (message "%s minutes and %s seconds left" minutes seconds))
    (message "No rest timer active")))

(defun rest-timer-cancel ()
  "Cancel running rest timer."
  (interactive)
  (when rest-timer--timer
    (cancel-timer rest-timer--timer)
    (setq rest-timer--timer nil)))

(defun rest-timer-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in.

There are three options: `my-org-clock-default-effort`. If there
is pre-defined effort in the current heading, do nothing.
Otherwise, prompt for choice.

The condition to run is when Effort is greater than the threshold
defined by the user.
"
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            my-org-clock-default-effort)))
      (unless (equal effort "")
        (org-set-property "Effort" effort))))
  (if (> (string-to-number
          (org-entry-get (point) "Effort")) rest-timer-threshold)
      (let ((current-prefix-arg rest-timer-threshold))
  (call-interactively #'rest-timer))))

(provide 'rest-timer)
;;; rest-timer.el ends here
