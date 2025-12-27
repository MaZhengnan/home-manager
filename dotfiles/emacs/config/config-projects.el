;; -*- coding: utf-8; lexical-binding: t; -*-
;;; projects.el 
;;; Commentary:
;;; Code:

;;; -*- lexical-binding: t; -*-

(use-package project
  :ensure nil  
  :demand t   
  :custom
  ;; 基本配置
  (project-switch-use-entire-map t)
  (project-mode-line '(:eval (when-let ((proj (project-current)))
                               (concat " [" (project-root proj) "]"))))
  
  ;; 项目根目录识别标记
  (project-vc-extra-root-markers '(".tool-versions" ".python-version" ".node-version"))
  
  ;; 项目列表排序
  (project-list-file (expand-file-name "projects" user-emacs-directory))
  
  ;; 项目切换配置
  (project-switch-commands 'project-find-file)  ; 按 p 默认执行 find-file
  
  ;; 忽略某些目录
  (project-vc-ignore-dir-regexp "\\(?:\\`\\.[^./]+\\)\\|\\(?:\\.git\\|node_modules\\|__pycache__\\|target\\|build\\|dist\\)\\'")
  
  :config
  ;; 1. 定义项目识别器 - 这是核心配置
  (defun my/project-try-local (dir)
    "检测各种项目类型的本地识别器"
    (let* ((root (project--find-root-internal
                  '(;; Git 项目
                    ".git"
                    ;; GitHub Actions
                    ".github"
                    ;; CMake 项目
                    "CMakeLists.txt"
                    "meson.build"
                    ;; Python 项目
                    "setup.py" "pyproject.toml" "requirements.txt" "Pipfile"
                    ;; Rust 项目
                    "Cargo.toml" "Cargo.lock"
                    ;; Qt 项目
                    "*.pro" "*.pri" "*.qrc"
                    ;; JavaScript/Node.js 项目
                    "package.json" "yarn.lock" "pnpm-lock.yaml"
                    ;; Go 项目
                    "go.mod" "go.sum"
                    ;; Ruby 项目
                    "Gemfile"
                    ;; 通用构建系统
                    "Makefile" "Dockerfile" ".dockerignore"
                    ;; 配置文件
                    ".projectile" ".gitignore"
                    ) dir)))
      (when root
        (cons 'local root))))
  
  ;; 2. 注册项目识别器到 project-find-functions
  (cl-pushnew 'my/project-try-local project-find-functions)
  
 ;; 5. 自定义 project-switch-project 的显示
  (defun my/project-switch-project-format (project)
    "自定义项目切换列表的显示格式"
    (let* ((root (car project))
           (name (file-name-nondirectory (directory-file-name root)))
           (type (cond
                  ((file-exists-p (expand-file-name ".git" root)) "Git")
                  ((file-exists-p (expand-file-name "Cargo.toml" root)) "Rust")
                  ((file-exists-p (expand-file-name "CMakeLists.txt" root)) "CMake")
                  ((file-exists-p (expand-file-name "pyproject.toml" root)) "Python")
                  ((file-exists-p (expand-file-name "package.json" root)) "Node.js")
                  (t "Project"))))
      (format "%-20s [%s]" name type)))
  
  ;; 6. 如果使用 vertico 等补全框架，可以设置显示格式
  (when (boundp 'vertico-sort-function)
    (setq vertico-sort-function nil))  ; 保持项目列表顺序
  
  ;; 7. 添加项目发现钩子（可选）
  (defun my/project-discovered-hook (project)
    "当发现新项目时执行的钩子"
    (message "Discovered project: %s" (project-root project)))
  
  (add-hook 'project-find-functions-hook #'my/project-discovered-hook)
  
  ;; 8. 项目相关的小工具函数
  (defun my/project-root-or-current-dir ()
    "获取当前项目根目录，如果没有则返回当前目录"
    (or (when-let ((proj (project-current)))
          (project-root proj))
        default-directory))
  
  (defun my/project-open-in-external-app ()
    "在外部应用中打开当前项目"
    (interactive)
    (let ((root (my/project-root-or-current-dir)))
      (cond
       ((eq system-type 'darwin) (shell-command (format "open '%s'" root)))
       ((eq system-type 'gnu/linux) (shell-command (format "xdg-open '%s'" root)))
       ((eq system-type 'windows-nt) (shell-command (format "start \"\" \"%s\"" root))))))
  
  (defun my/project-find-file-other-window ()
    "在其他窗口打开项目文件"
    (interactive)
    (let ((project-current (project-current)))
      (if project-current
          (project-find-file-in nil (project-root project-current) t)
        (call-interactively 'find-file-other-window))))
 
  ;; 10. 项目模式下的额外配置
  (define-minor-mode my-project-mode
    "项目模式的次要模式"
    :global nil
    :lighter " Proj"
    (if my-project-mode
        (progn
          ;; 启用项目特定的设置
          (setq-local compilation-directory (project-root (project-current)))
          (message "Project mode enabled for %s" (project-root (project-current))))
      (message "Project mode disabled")))
  
  ;; 自动启用项目模式
  (add-hook 'project-find-functions-hook
            (lambda (_project)
              (my-project-mode 1)))
  )
  
(provide 'config-projects)
