;;; fastframe.el --- Mitigate frame creation performance. -*- lexical-binding: t; -*-

;; Filename: fastframe.el
;; Description: Mitigate frame creation performance.
;; Author: jakanakaevangeli <jakanakaevangeli@chiru.no>
;; Created: 2021-04-23
;; Version: 1.0
;; Keywords: frames
;; URL: https://github.com/jakanakaevan/emacs-fastframe

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

;; This is a tiny tool to mitigate make-frame performance issues.
;;
;; Graphical frame creation can get very slow when a lot of faces are defined.
;; Fastframe can help mitigate this by creating a pool of invisible frames
;; during idle time. When a frame is requested with `frame-creation-function',
;; a pre-made frame will be made visible from the pool and returned.

;;; Code:

(defgroup fastframe nil
  "Mitigate frame creation performance."
  :group 'frames
  :prefix "fastframe-"
  :link '(url-link "https://github.com/jakanakaevan/emacs-fastframe"))

(defcustom fastframe-pool-size 15
  "Maximum number of invisible frames to keep in the pool."
  :type 'integer)

(defcustom fastframe-idle-time 2
  "Time in seconds to wait before we start filling frame pool.
If the number of invisible frames in the frame pool is smaller
than `fastframe-pool-size', start create new frames after this
amount of idle time."
  :type 'number)

(defvar fastframe--pool nil
  "Alist of invisible frames.
Cars are frame parameters and cdrs are lists of invisible
frames.")

(defvar fastframe--pool-current-count 0
  "Number of invisible frames in `fastframe--pool'.")

(defvar fastframe--timer nil)

(defun fastframe-x-create-frame-with-faces (parameters)
  "Create and return a frame with frame parameters PARAMETERS.
Like `x-create-frame-with-faces', but tries to reuse existing
invisible frames from `fastframe--pool' with matching
PARAMETERS."
  (let ((as (assoc parameters fastframe--pool))
        (frame nil))
    (if as
        (while (and (null frame) (cadr as))
          (let ((inhibit-quit t))
            (setq frame (pop (cdr as)))
            (setq fastframe--pool-current-count
                  (1- fastframe--pool-current-count)))
          (if (frame-live-p frame)
              (make-frame-visible frame)
            (setq frame nil)))
      (setq as (list parameters))
      (push as fastframe--pool))
    (prog1 (or frame (x-create-frame-with-faces parameters))
      (fastframe--setup-timer as))))

(defun fastframe--setup-timer (assoc)
  "Set up a timer to start making frames for the pool.
Use frame parameters specified in car of ASSOC and add frames to
cdr of ASSOC."
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
                   (fastframe--add-frame-to-assoc assoc))
               (when (fastframe--remove-random-from-pool assoc)
                 (fastframe--add-frame-to-assoc assoc))
               (cancel-timer timer)
               (setq fastframe--timer nil))
             (sit-for fastframe-idle-time))))))
    (setq fastframe--timer timer)))

(defun fastframe--remove-random-from-pool (assoc)
  "Remove a random frame from the pool.
Do nothing and return nil if it corresponds to ASSOC from
`frame--pool'. Otherwise remove it and return t."
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

(defun fastframe--add-frame-to-assoc (assoc)
  "Add a frame to cdr of ASSOC.
car of ASSOC specifies the frames parameters. Increase
`fastframe--pool-current-count'."
  (when (memq window-system '(x w32 ns))
    (let* ((frame (x-create-frame-with-faces
                   (cons '(visibility . nil) (car assoc)))))
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
  (with-eval-after-load 'w32-win
    (cl-defmethod frame-creation-function (params &context (window-system ns))
      (fastframe-x-create-frame-with-faces params))))

(provide 'fastframe)
;;; fastframe.el ends here
