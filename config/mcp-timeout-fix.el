;;; mcp-timeout-fix.el --- Forward MCP timeouts to tool callers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Adapted from mcp.el, Copyright (C) 2025 lizqwer scott.

;;; Commentary:
;; Temporary workaround for mcp-async-call-tool omitting :timeout-fn.
;; Load with M-x load-file, or load this file from your init after mcp is
;; available.  Loading installs the workaround for future tool requests.
;; Requests that have already timed out cannot be recovered by this file.
;; It preserves the connection's configured timeout; it does not retry or
;; cancel work on the remote server.  It only addresses lost local timeouts,
;; not exceptions in response parsing or other tool implementations.
;; To remove: M-x mcp-timeout-fix-disable.

;;; Code:

(require 'mcp)

(defun mcp-timeout-fix--call-tool
    (connection name arguments callback error-callback)
  "Call MCP tool NAME, forwarding local timeouts to ERROR-CALLBACK.
CONNECTION, ARGUMENTS and CALLBACK follow `mcp-async-call-tool'."
  (let ((timeout (mcp--timeout connection)))
    (jsonrpc-async-request
     connection :tools/call
     (list :name name :arguments (or arguments (make-hash-table)))
     :timeout timeout
     :success-fn callback
     :error-fn (jsonrpc-lambda (&key code message _data)
                 (funcall error-callback code message))
     :timeout-fn
     (lambda ()
       (funcall
        error-callback 124
        (format (concat "Local MCP timeout: %s did not return within %s seconds. "
                        "The remote operation may still have completed; "
                        "verify its effects before retrying.")
                name timeout))))))

(defun mcp-timeout-fix-disable ()
  "Remove the workaround for future calls."
  (interactive)
  (advice-remove 'mcp-async-call-tool #'mcp-timeout-fix--call-tool))

(advice-add 'mcp-async-call-tool :override #'mcp-timeout-fix--call-tool)

(provide 'mcp-timeout-fix)
;;; mcp-timeout-fix.el ends here
