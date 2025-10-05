;;; .dir-locals.el --- Directory-local variables for RacketCon 2025 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 RacketCon

;; This file loads the main RacketCon 2025 configuration.
;; All project-specific settings are in racketcon-2025-config.el

;;; Code:

((nil . ((eval . (let ((config-file (expand-file-name "racketcon-2025-config.el"
                                                       (locate-dominating-file
                                                        default-directory
                                                        ".dir-locals.el"))))
                   (when (file-exists-p config-file)
                     (load config-file)))))))

;;; .dir-locals.el ends here
