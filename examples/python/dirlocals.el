((nil . (
         (eval . (progn
                   (add-hook 'python-ts-mode-hook #'lsp-deferred)
                   (add-hook 'lsp-mode-hook #'lsp-lens-mode)

                   (with-eval-after-load 'lsp-mode
                     (lsp-register-client
                      (make-lsp-client
                       :new-connection
                       (lsp-stdio-connection
                        '("basedpyright-langserver" "--stdio"))
                       :activation-fn (lsp-activate-on "python")
                       :major-modes '(python-mode python-ts-mode)
                       :priority 1
                       :multi-root t
                       :server-id 'basedpyright))

                     ;; Ruff: linting, formatting, and code actions
                     (lsp-register-client
                      (make-lsp-client
                       :new-connection
                       (lsp-stdio-connection '("ruff" "server"))
                       :activation-fn (lsp-activate-on "python")
                       :major-modes '(python-mode python-ts-mode)
                       :add-on? t
                       :priority -1
                       :multi-root t
                       :server-id 'ruff))
                     )
                   
                   (dap-register-debug-template "Python :: Streamlit Archimind"
                                                (list :type "python" :debugger "debugpy" :request
                                                      "launch" :name
                                                      "Python :: Streamlit Archimind" :module
                                                      "streamlit" :cwd
                                                      "/Users/s.vanie/projects/archimind-graphrag"
                                                      :program "run" :args
                                                      '("archimind_graphrag/app/app.py"
                                                        "--server.port=8501"
                                                        "--server.address=0.0.0.0")
                                                      :justMyCode t))
                   (require 'dap-python)))
         ))
 )
