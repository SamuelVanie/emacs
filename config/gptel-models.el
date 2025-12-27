(gptel-make-gemini "Gemini"
  :key (with-temp-buffer (insert-file-contents "~/.org/.gem_key") (string-trim (buffer-string)))
  :stream t)

(gptel-make-deepseek "DeepSeek"       ;Any name you want
  :stream t                           ;for streaming responses
  :key (with-temp-buffer (insert-file-contents "~/.org/.deep_key") (string-trim (buffer-string))))

(gptel-make-openai "qwen"
  :host "dashscope-intl.aliyuncs.com"
  :endpoint "/compatible-mode/v1/chat/completions"
  :protocol "https"
  :key (with-temp-buffer (insert-file-contents "~/.org/.qw_key") (string-trim (buffer-string)))
  :models '("qwen3-coder-plus" "qwen-plus-latest"))

(gptel-make-openai "zai"
  :host "api.z.ai"
  :endpoint "/api/coding/paas/v4/chat/completions"
  :protocol "https"
  :key (with-temp-buffer (insert-file-contents "~/.org/.zai_key") (string-trim (buffer-string)))
  :models '("glm-4.7" "glm-4.5-air"))

(setq gptel-backend
      (gptel-make-openai "OpenRouter"
      	;; :online in the language slug to add the search plugin
      	:host "openrouter.ai"
      	:endpoint "/api/v1/chat/completions"
      	:stream t
      	:key (with-temp-buffer (insert-file-contents "~/.org/.openr_key") (string-trim (buffer-string)))
      	:models '(
      		  (anthropic/claude-haiku-4.5 :input-cost 1 :output-cost 5)
      		  (anthropic/claude-sonnet-4.5 :input-cost 3 :output-cost 10)
      		  (deepseek/deepseek-v3.2 :input-cost 0.224 :output-cost 0.32)
      		  (google/gemini-2.5-flash-lite :input-cost 0.10 :output-cost 0.4)
      		  (google/gemini-3-flash-preview :input-cost 0.5 :output-cost 3)
      		  (google/gemini-3-pro-preview :input-cost 2 :output-cost 12)
      		  (minimax/minimax-m2.1 :input-cost 0.3 :output-cost 1.2)
      		  (moonshotai/kimi-dev-72b :input-cost 0.29 :output-cost 1.15)
      		  (moonshotai/kimi-k2-thinking :input-cost 0.4 :output-cost 1.75)
      		  (openai/gpt-4.1 :input-cost 2 :output-cost 8)
      		  (openai/gpt-5.2 :input-cost 1.75 :output-cost 14)
      		  (openai/gpt-5.1-codex :input-cost 1.25 :output-cost 10)
      		  (qwen/qwen3-coder :input-cost 0.22 :output-cost 0.95)
      		  (qwen/qwen3-coder-flash :input-cost 0.3 :output-cost 1.50)
      		  (qwen/qwen3-coder-plus :input-cost 1 :output-cost 5)
      		  (switchpoint/router :input-cost 0.85 :output-cost 3.40)
      		  (x-ai/grok-code-fast-1 :input-cost 0.2 :output-cost 1.5)
      		  (x-ai/grok-4.1-fast :input-cost 0.2 :output-cost 0.5)
      		  (z-ai/glm-4.7 :input-cost 0.4 :output-cost 1.5)
      		  ))
      )

(gptel-make-anthropic "Anthropic"
  :key (with-temp-buffer (insert-file-contents "~/.org/.ant_key") (string-trim (buffer-string)))
  :stream t)

(gptel-make-gh-copilot "Copilot")

;; ;; local models
(gptel-make-openai "lmstudio"
  :host "127.0.0.1:1234"
  :endpoint "/v1/chat/completions"
  :protocol "http"
  :stream t
  :key "dummy"
  :models '(
      	    qwen/qwen2.5-coder-14b
      	    deepseek-coder-6.7b-instruct
      	    qwen/qwen3-vl-8b
      	    essentialai/rnj-1
            ))
