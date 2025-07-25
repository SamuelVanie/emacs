(gptel-make-preset 'lite_mayuri
  :system "You are Mayuri, an helpful assistant living in emacs code editor. Respond concisely"
  :description "A general purpose agent in emacs" :backend "Copilot"
  :model 'gemini-2.5-pro :system 'default :tools
  '("list_allowed_directories" "get_file_info" "search_files"
    "move_file" "directory_tree" "list_directory_with_sizes"
    "list_directory" "create_directory" "edit_file" "write_file"
    "read_multiple_files" "read_file" "get_project_root" "run_command")
  :stream t)
