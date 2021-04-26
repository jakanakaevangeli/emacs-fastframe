;;; fastframe.el --- Mitigate frame creation performance. -*- lexical-binding: t; -*-

;; Filename: fastframe.el
;; Description: Mitigate frame creation performance.
;; Author: jakanakaevangeli <jakanakaevangeli@chiru.no>
;; Created: 2021-04-23
;; Version: 1.0
;; Keywords: frames
;; URL: https://github.com/jakanakaevangeli/emacs-fastframe

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple tool to mitigate make-frame performance issues.
;;
;; Graphical frame creation can get very slow when a lot of faces are defined.
;; Fastframe can help mitigate this by creating a pool of invisible frames
;; during idle time. When a frame is requested with `frame-creation-function',
;; a pre-made frame will be made visible from the pool and returned.
;;
;; Installation:
;;
;; Add the following to you Emacs init file:
;;
;;  (add-to-list 'load-path "/path/to/emacs-fastframe")
;;  (require 'fastframe)
;;  (fastframe-activate)
;;
;; Notes:
;;
;; - The frame pool is only used for creation of graphical frames, creation of
;;   tty frames is unaffected.
;; - Doesn't work well if using multiple GUI display servers/terminals at once
;; - I see this as a temporary kludge until
;;   https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41200 is fixed

;;; Code:

(defgroup fastframe nil
  "Mitigate frame creation performance."
  :group 'frames
  :prefix "fastframe-"
  :link '(url-link "https://github.com/jakanakaevangeli/emacs-fastframe"))

(defcustom fastframe-pool-size 15
  "Maximum number of invisible frames to keep in the pool."
  :type 'integer)

(defcustom fastframe-idle-time 2
  "Time in seconds to wait before we start filling frame pool.
If the number of invisible frames in the frame pool is smaller
than `fastframe-pool-size', start creating new frames after this
amount of idle time."
  :type 'number)

(defvar fastframe-static-parameters
  '(display display-type name minibuffer window-id outer-window-id
            font-backend)
  "List of frame parameters that cannot be changed.
Fastframe will not try to change these parameters of frames in
the pool. It will instead choose a frame that was created with
these parameters if it exist.")

(defvar fastframe--pool nil
  "Alist of invisible frames.
Cars are frame parameters and cdrs are lists of invisible frames.
Only frame parameters from `fastframe-static-parameters' are used
for the cars.")

(defvar fastframe--pool-current-count 0
  "Number of invisible frames in `fastframe--pool'.")

(defvar fastframe--timer nil)

(defvar fastframe--buffer nil
  "Buffer displayed on invisible frames.")

(defun fastframe-x-create-frame-with-faces (parameters)
  "Create and return a frame with frame parameters PARAMETERS.
Like `x-create-frame-with-faces', but tries to reuse existing
invisible frames from `fastframe--pool' with matching
PARAMETERS."
  (let* ((static-params fastframe-static-parameters)
         stripped-params as frame
         specified-visibility specified-no-other-frame)
    (dolist (param parameters)
      (let ((param-name (car param)))
        (when (memq param-name static-params)
          (push param stripped-params))
        (cond ((eq param-name 'visibility)
               (setq specified-visibility t))
              ((eq param-name 'no-other-frame)
               (setq specified-no-other-frame t)))))

    (if (setq as (assoc stripped-params fastframe--pool))
        (while (and (null frame) (cadr as))
          (let ((inhibit-quit t))
            (setq frame (pop (cdr as)))
            (setq fastframe--pool-current-count
                  (1- fastframe--pool-current-count)))
          (if (frame-live-p frame)
              (progn
                (unless specified-visibility
                  (setq parameters (cons '(visibility . t) parameters)))
                (unless specified-no-other-frame
                  (setq parameters (cons '(no-other-frame . nil) parameters)))
                (modify-frame-parameters frame parameters))
            (setq frame nil)))
      (setq as (list stripped-params))
      (push as fastframe--pool))
    (prog1 (or frame (x-create-frame-with-faces parameters))
      (fastframe--setup-timer as parameters))))

(defun fastframe--setup-timer (assoc params)
  "Set up a timer to start making frames for the pool.
Use frame parameters PARAMS and add frames to cdr of ASSOC."
  (let ((timer fastframe--timer))
    (when timer (cancel-timer timer))
    (setq
     timer
     (run-with-idle-timer
      fastframe-idle-time t
      (lambda ()
        (while
            (and
             (eq timer fastframe--timer)
             (if (< fastframe--pool-current-count fastframe-pool-size)
                 (prog1 t
                   (fastframe--add-frame-to-assoc assoc params))
               (when (fastframe--remove-random-from-pool assoc)
                 (fastframe--add-frame-to-assoc assoc params))
               (cancel-timer timer)
               (setq fastframe--timer nil))
             (sit-for fastframe-idle-time))))))
    (setq fastframe--timer timer)))

(defun fastframe--remove-random-from-pool (assoc)
  "Remove a random frame from the pool.
Do nothing and return nil if it corresponds to ASSOC from
`frame--pool'. Otherwise remove it, return t and lower
`fastframe--pool-current-count'."
  (let* ((rand (random fastframe--pool-current-count))
         (tail fastframe--pool)
         (deleted nil))
    (while
        (and
         (consp tail)
         (<= 0 rand)
         (let ((len (length (cdar tail))))
           (if (< rand len)
               (prog1 nil
                 (unless (eq (car tail) assoc)
                   (delete-frame
                    (let ((inhibit-quit t))
                      (setq fastframe--pool-current-count
                            (1- fastframe--pool-current-count))
                      (pop (cdar tail))))
                   (setq deleted t)))
             (setq rand (- rand len)))))
      (setq tail (cdr tail)))
    deleted))

(defun fastframe--add-frame-to-assoc (assoc params)
  "Create an invisible frame and add it to cdr of ASSOC.
Use PARAMS as the frame's parameters. Increase
`fastframe--pool-current-count'."
  (when (memq window-system '(x w32 ns))
    (let ((buffer fastframe--buffer)
          (frame nil))
      (unless (buffer-live-p buffer)
        (setq buffer (setq fastframe--buffer
                           (generate-new-buffer " *fastframe*"))))
      (with-current-buffer buffer
        (unwind-protect
            (progn
              ;; `x-create-frame' displays the current buffer on newly created
              ;; frame if its name doesn't begin with a space
              (rename-buffer "*fastframe*" t)
              (setq frame
                    (x-create-frame-with-faces
                     `((no-other-frame . t) (visibility . nil) ,@params))))
          (rename-buffer " *fastframe*" t)))
      (let ((inhibit-quit t))
        (push frame (cdr assoc))
        (setq fastframe--pool-current-count
              (1+ fastframe--pool-current-count))))))

;;;###autoload
(defun fastframe-activate ()
  "Activate fastframe."
  (with-eval-after-load 'x-win
    (cl-defmethod frame-creation-function (params &context (window-system x))
      (fastframe-x-create-frame-with-faces params)))
  (with-eval-after-load 'w32-win
    (cl-defmethod frame-creation-function (params &context (window-system w32))
      (fastframe-x-create-frame-with-faces params)))
  (with-eval-after-load 'ns-win
    (cl-defmethod frame-creation-function (params &context (window-system ns))
      (fastframe-x-create-frame-with-faces params))))

(provide 'fastframe)
;;; fastframe.el ends here
