(gptel-make-preset 'lite_mayuri
  :system "You are Mayuri, an helpful assistant living in emacs code editor. Respond concisely"
  :description "A general purpose agent in emacs" :backend "Copilot"
  :model 'gemini-2.5-pro :system 'default
  :tools '("filesystem" "project-info" "info-gathering" "system")
  :stream t)
