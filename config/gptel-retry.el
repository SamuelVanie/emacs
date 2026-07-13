;;; gptel-retry.el --- Bounded, status-aware retries for gptel -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup smv/gptel-retry nil
  "Retry transient gptel request failures."
  :group 'gptel)

(defcustom smv/gptel-retry-initial-delay 2.0
  "Initial retry delay, in seconds."
  :type 'number)

(defcustom smv/gptel-retry-maximum-delay 60.0
  "Maximum computed retry delay, in seconds.

This cap applies to locally computed exponential backoff.  A server-provided
Retry-After value should normally take precedence, if it is made available by
gptel."
  :type 'number)

(defcustom smv/gptel-retry-maximum-attempts 3
  "Maximum number of retries for one logical request.

The initial request is not counted as a retry.  Thus, a value of 5 permits at
most six total attempts."
  :type 'integer)

(defcustom smv/gptel-retry-jitter 0.25
  "Amount of proportional random jitter added to retry delays.

For example, 0.25 varies a computed delay by approximately plus or minus 25
percent.  Jitter prevents several buffers or clients from retrying at exactly
the same time."
  :type 'number)

(defcustom smv/gptel-retry-statuses
  '(408 425 429 500 502 503 504 529)
  "HTTP statuses that are normally safe to retry.

529 is used by Anthropic for overloaded API responses.  Status 409 is omitted
deliberately: some APIs use it for transient conflicts, while others use it for
permanent request-state errors."
  :type '(repeat integer))

(defvar-local smv/gptel-retry-attempt 0
  "Number of consecutive retry attempts in the current buffer.")

(defvar-local smv/gptel-retry-timer nil
  "Pending retry timer for the current buffer.")

(defvar-local smv/gptel-retry-generation 0
  "Generation used to invalidate retries belonging to older requests.")

(defun smv/gptel-retry--http-status (info)
  "Return INFO's HTTP status as an integer, or nil.

gptel currently exposes `:http-status' as a string, but accepting integers
makes this code tolerant of future changes."
  (let ((status (plist-get info :http-status)))
    (cond
     ((integerp status) status)
     ((and (stringp status)
           (string-match-p "\\`[0-9]+\\'" status))
      (string-to-number status))
     (t nil))))

(defun smv/gptel-retry--retryable-p (status)
  "Return non-nil when STATUS represents a transient failure.

A nil STATUS is treated as a transport-level failure, such as a timeout,
connection reset, DNS failure, or TLS interruption.  Such failures remain
bounded by `smv/gptel-retry-maximum-attempts'."
  (or (null status)
      (memq status smv/gptel-retry-statuses)))

(defun smv/gptel-retry--delay (attempt)
  "Return the jittered retry delay for ATTEMPT.

ATTEMPT starts at one for the first retry."
  (let* ((base
          (min smv/gptel-retry-maximum-delay
               (* smv/gptel-retry-initial-delay
                  (expt 2 (max 0 (1- attempt))))))
         ;; Produce a value in approximately [-1.0, 1.0].
         (unit-random (- (* 2.0 (/ (float (random 1000000)) 1000000.0))
                         1.0))
         (factor (+ 1.0 (* smv/gptel-retry-jitter unit-random))))
    ;; Never let jitter produce a negative delay.
    (max 0.1 (* base factor))))

(defun smv/gptel-retry-cancel ()
  "Cancel any pending gptel retry in the current buffer."
  (interactive)
  (when (timerp smv/gptel-retry-timer)
    (cancel-timer smv/gptel-retry-timer))
  (setq smv/gptel-retry-timer nil
        smv/gptel-retry-attempt 0)
  ;; Invalidate callbacks that may already have started.
  (cl-incf smv/gptel-retry-generation))

(defun smv/gptel-retry-reset (&rest _)
  "Reset retry state in the current buffer.

This can be attached to a successful-completion handler or called before a new
user-initiated request."
  (when (timerp smv/gptel-retry-timer)
    (cancel-timer smv/gptel-retry-timer))
  (setq smv/gptel-retry-timer nil
        smv/gptel-retry-attempt 0))

(defun smv/gptel-retry-request (fsm)
  "Retry a transient failed request represented by FSM.

Retries use bounded exponential backoff with jitter.  Permanent HTTP failures
are reported without retrying."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer))
         (position (plist-get info :position))
         (status (smv/gptel-retry--http-status info)))
    (cond
     ((not (buffer-live-p buffer))
      (message "gptel: request failed, but its source buffer no longer exists"))

     ((not (smv/gptel-retry--retryable-p status))
      (with-current-buffer buffer
        (smv/gptel-retry-reset))
      (message "gptel: not retrying non-transient HTTP error %s"
               (or status "unknown")))

     (t
      (with-current-buffer buffer
        (if (>= smv/gptel-retry-attempt smv/gptel-retry-maximum-attempts)
            (progn
              (smv/gptel-retry-reset)
              (message
               "gptel: giving up after %d retries%s"
               smv/gptel-retry-maximum-attempts
               (if status
                   (format " (last HTTP status %d)" status)
                 " (last failure was a transport error)")))

          (cl-incf smv/gptel-retry-attempt)

          ;; Replace an already scheduled retry rather than allowing duplicate
          ;; callbacks to resend the same request.
          (when (timerp smv/gptel-retry-timer)
            (cancel-timer smv/gptel-retry-timer))

          (let* ((attempt smv/gptel-retry-attempt)
                 (delay (smv/gptel-retry--delay attempt))
                 (generation smv/gptel-retry-generation)
                 ;; Copy the marker because the FSM's marker may subsequently
                 ;; be changed or discarded.
                 (marker
                  (cond
                   ((markerp position) (copy-marker position))
                   ((integer-or-marker-p position)
                    (copy-marker position))
                   (t nil))))
            (setq
             smv/gptel-retry-timer
             (run-at-time
              delay nil
              (lambda (buf pos expected-generation retry-number)
                (unwind-protect
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (setq smv/gptel-retry-timer nil)
                        ;; Do not resend an obsolete request after retry state
                        ;; has been cancelled or replaced.
                        (when (= expected-generation
                                 smv/gptel-retry-generation)
                          (when (and (markerp pos)
                                     (eq buf (marker-buffer pos)))
                            (goto-char pos))
                          (message
                           "gptel: retrying request now (attempt %d/%d)"
                           retry-number
                           smv/gptel-retry-maximum-attempts)
                          (condition-case err
                              (gptel-send)
                            (error
                             ;; Errors thrown synchronously by `gptel-send'
                             ;; do not necessarily reach the ERRS FSM handler.
                             ;; Reset here rather than creating a timer loop.
                             (smv/gptel-retry-reset)
                             (message
                              "gptel: retry could not be started: %s"
                              (error-message-string err)))))))
                  (when (markerp pos)
                    (set-marker pos nil))))
              buffer marker generation attempt))

            (message
             "gptel: transient%s failure; retry %d/%d scheduled in %.1fs"
             (if status (format " HTTP %d" status) " transport")
             attempt
             smv/gptel-retry-maximum-attempts
             delay))))))))

;; Install without adding duplicate handlers when the configuration is
;; evaluated more than once.
(cl-pushnew #'smv/gptel-retry-request
            (alist-get 'ERRS gptel-send--handlers))


(defun smv/gptel-retry-cancel-on-abort (&rest _)
  "Cancel pending retry state before `gptel-abort' runs."
  (smv/gptel-retry-cancel))

;; Avoid installing the advice more than once when reloading the config.
(unless (advice-member-p #'smv/gptel-retry-cancel-on-abort #'gptel-abort) (advice-add #'gptel-abort :before #'smv/gptel-retry-cancel-on-abort))
