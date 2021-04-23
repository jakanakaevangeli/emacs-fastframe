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

(defvar fastframe--timer nil)

(defun fastframe-x-create-frame-with-faces (parameters)
  "Create and return a frame with frame parameters PARAMETERS.
Like `x-create-frame-with-faces', but tries to reuse existing
invisible frames from `fastframe--pool' with matching
PARAMETERS."
  (let ((as (assoc parameters fastframe--pool))
        (frame nil))
    (if as
        (if (setq frame (cadr as))
            (progn
              (let ((inhibit-quit t))
                (pop (cdr as))
                (setq fastframe--pool-current-count
                      (1- fastframe--pool-current-count)))
              (make-frame-visible frame)))
      (setq as (list parameters))
      (push as fastframe--pool))
    (unless frame
      (setq frame (x-create-frame-with-faces parameters)))

    (when-let* ((timer fastframe--timer))
      (cancel-timer timer))
    (let ((timer))
      (setq
       timer
       (run-with-idle-timer
        fastframe-idle-time t
        (lambda ()
          (while
              (and
               (eq timer fastframe--timer)
               (progn
                 (if (< fastframe--pool-current-count
                        fastframe-pool-size)
                     (prog1 t
                       (fastframe--add-frame-to-assoc as))
                   (when (fastframe--remove-random-from-pool as)
                     (fastframe--add-frame-to-assoc as))
                   (cancel-timer timer)
                   (setq fastframe--timer nil)))
               (sit-for fastframe-idle-time))))))
      (setq fastframe--timer timer))
    frame))

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
  "Add a frame with to cdr of ASSOC.
car of ASSOC specifies the frames parameters. Increase
`fastframe--pool-current-count'."
  (when (memq window-system '(x w32 ns))
    (when (< fastframe--pool-current-count fastframe-pool-size)
      (let* ((frame (x-create-frame-with-faces
                     (cons '(visibility . nil) (car assoc)))))
        (let ((inhibit-quit t))
          (push frame (cdr assoc))
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
