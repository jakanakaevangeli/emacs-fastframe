;; -*- lexical-binding: t; -*-

(defgroup fastframe nil
  "Faster make-frame using a pool invisible pre-made frames."
  :group 'frames
  :prefix "fastframe-"
  :link '(url-link "https://github.com/jakanakaevan/fastframe.el"))

(defcustom fastframe-pool-size 10
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

(defvar fastframe--to-remove nil)
(defvar fastframe--last-parameters nil)

(defun fastframe-x-create-frame-with-faces (parameters)
  "Create and return a frame with frame parameters PARAMETERS.
Like `x-create-frame-with-faces', but tries to reuse existing
invisible frames from `fastframe--pool' with matching
PARAMETERS."
  (prog1
      (if-let* ((as (assoc parameters fastframe--pool))
                (frame (cadr as)))
          (progn
            (let ((inhibit-quit t))
              (pop (cdr as))
              (setq fastframe--pool-current-count
                    (1- fastframe--pool-current-count)))
            (make-frame-visible frame)
            frame)
        (x-create-frame-with-faces parameters))
    (setq fastframe--to-remove t)
    (setq fastframe--last-parameters parameters)))

;;;###autoload
(defun fastframe--timer-function ()
  "Add a new invisible frame to pool if necessary.
Is the number of frames in the pool is less than
`fastframe-pool-size', add a new frame and run this function
again in a timer. If `fastframe--to-remove' in non-nil, first
remove a random frame that doesn't match
`fastframe--last-parameters'."
  (when (memq window-system '(x w32 ns))
    (when (and fastframe--to-remove
               (=> fastframe--pool-current-count fastframe-pool-size))
      (let* ((keep (assoc fastframe--last-parameters fastframe--pool))
             (rand (random
                    (- fastframe--pool-current-count (length (cdr keep)))))
             (tail fastframe--pool))
        (while
            (and
             (consp tail)
             (<= 0 rand)
             (or (eq (car tail) keep)
                 (let ((len (length (cdar tail))))
                   (if (< rand len)
                       (prog1 nil
                         (delete-frame
                          (let ((inhibit-quit t))
                            (setq fastframe--pool-current-count
                                  (1- fastframe--pool-current-count))
                            (pop (cdar tail)))))
                     (setq rand (- rand len))))))
          (setq tail
                (cdr tail)))))
    (setq fastframe--to-remove nil)
    (when (< fastframe--pool-current-count fastframe-pool-size)
      (let* ((params fastframe--last-parameters)
             (frame (x-create-frame-with-faces
                     (cons '(visibility . nil) params)))
             (as (assoc params fastframe--pool)))
        (unless as
          (setq as (list params))
          (push as fastframe--pool))
        (let ((inhibit-quit t))
          (push frame (cdr as))
          (setq fastframe--pool-current-count
                (1+ fastframe--pool-current-count)))))))

;;;###autoload
(defun fastframe-activate ()
  (with-eval-after-load 'x-win
    (cl-defmethod frame-creation-function (params &context (window-system x))
      (fastframe-x-create-frame-with-faces params)))
  (with-eval-after-load 'w32-win
    (cl-defmethod frame-creation-function (params &context (window-system w32))
      (fastframe-x-create-frame-with-faces params)))
  (with-eval-after-load 'w32-win
    (cl-defmethod frame-creation-function (params &context (window-system ns))
      (fastframe-x-create-frame-with-faces params)))
  (run-with-idle-timer fastframe-idle-time t
                       #'fastframe--timer-function))

(provide 'fastframe)
