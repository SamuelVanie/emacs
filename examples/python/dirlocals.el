((nil . (
         (eval . (progn
                   (add-hook 'python-ts-mode-hook #'lsp-deferred)
                   (add-hook 'lsp-mode-hook #'lsp-lens-mode)

                   (dap-register-debug-template
                    "Python :: Streamlit Archimind"
                    (list :type "python"
                          :debugger "debugpy"
                          :request "launch"
                          :name "Python :: Streamlit Archimind"
                          :module "streamlit"
                          :cwd "/Users/s.vanie/projects/archimind-graphrag"
                          :program "run"
                          :args '(
                                  "archimind_graphrag/app/app.py"
                                  "--server.port=8501"
                                  "--server.address=0.0.0.0")
                          :justMyCode t))
                                      
                   (require 'dap-python)
                   ))
         ))
 )
