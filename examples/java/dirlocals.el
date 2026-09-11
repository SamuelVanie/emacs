((eval . (setq projectile-project-run-cmd "mvn clean && mvn -Dstyle.color=always spring-boot:run | ~/.emacs.d/utilities/slf4j_jq_prettifier.sh"))
         (eval . (setq projectile-project-test-cmd "mvn test"))
         (eval . (setq projectile-project-configure-cmd "mvn clean"))
         (eval . (progn
                   (require 'lsp-java-boot)
                   (setq lsp-java-compile-null-analysis-mode "automatic")

                   (add-hook 'java-ts-mode-hook #'lsp-deferred)
                   (add-hook 'lsp-mode-hook #'lsp-lens-mode)
                   (add-hook 'java-ts-mode-hook #'lsp-java-boot-lens-mode)

                   (add-to-list 'projectile-tasks '("debug" . "mvn clean && mvn spring-boot:run -Dstyle.color=always -Dspring-boot.run.jvmArguments=\"-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=*:5003\" | ~/.emacs.d/utilities/slf4j_jq_prettifier.sh"))
                   
                   (with-eval-after-load 'lsp-java
                     (setq lsp-java-configuration-runtimes
                           '[(:name "JavaSE-21" :path
                                    "/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home"
                                    :default t)])
                     (setq lsp-java-jdt-download-url
                           "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.32.0/jdt-language-server-1.32.0-202402011424.tar.gz")
                     (let
                         ((lombok-jar
                           (expand-file-name
                            "/Users/vanie_s/.m2/repository/org/projectlombok/lombok/1.18.36/lombok-1.18.36.jar")))
                       (unless (file-exists-p lombok-jar)
                         (error "Lombok-Jar not found: %s" lombok-jar))
                       (add-to-list 'lsp-java-vmargs (concat "-javaagent:" lombok-jar))))))
         (java-ts-mode . ((java-ts-mode-offset . 2))))
