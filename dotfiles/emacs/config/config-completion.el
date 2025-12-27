;; -*- coding: utf-8; lexical-binding: t; -*-
;;; completion.el 

;; 基础补全框架
(use-package vertico
  :init
  (vertico-mode)
  :bind
  (:map minibuffer-local-map
        ("C-j" . next-line)
        ("C-k" . previous-line)
        :map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

;; 保存历史
(savehist-mode)

;; 顺序很重要：orderless 应该在 vertico 之后
(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; posframe 支持
(use-package vertico-posframe
  :ensure t
  :after (vertico orderless)
  :init
  (vertico-posframe-mode)
  :config
  (setq vertico-posframe-poshandler
        #'posframe-poshandler-frame-center
        vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)
          (width . 0.6)
          (min-width . 60)
          (max-width . 100)
          (height . 0.4)
          (min-height . 10)
          (max-height . 20))))

;; 注释增强
(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

;; 图标支持
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

;; 补全框架 - 重点修改这里
(use-package corfu
  :ensure t
  :after orderless
  :custom
  (corfu-cycle t)                ; 允许循环补全
  (corfu-auto t)                 ; 自动补全
  (corfu-auto-prefix 2)          ; 输入 2 个字符后自动补全
  (corfu-popupinfo-mode t)       ; 显示补全信息
  (corfu-popupinfo-delay 0.5)    ; 信息延迟显示
  (corfu-separator ?\s)          ; 分隔符
  (completion-ignore-case t)     ; 忽略大小写
  (tab-always-indent 'complete)  ; Tab 总是触发补全
  (corfu-preview-current nil)    ; 不预览当前项
  (corfu-quit-at-boundary t)     ; 到达边界时退出
  (corfu-quit-no-match t)        ; 没有匹配时退出
  (corfu-auto-delay 0.2)         ; 自动补全延迟
  :init
  :hook
  ((prog-mode text-mode) . corfu-mode)  ; 只在编程和文本模式启用
  (org-mode . (lambda () (corfu-mode -1)))  ; org-mode 禁用 (global-corfu-mode)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)     ; Tab 切换到下一个补全项
        ([tab] . corfu-next)     ; 同上
        ("<backtab>" . corfu-previous) ; Shift+Tab 切换到上一个
        ([backtab] . corfu-previous)   ; 同上
        ("S-TAB" . corfu-previous)     ; Shift+Tab
        ("RET" . corfu-insert)   ; 回车插入当前补全
        ("C-n" . corfu-next)     ; C-n 下一个
        ("C-p" . corfu-previous) ; C-p 上一个
        ("C-f" . corfu-scroll-up)    ; 滚动
        ("C-b" . corfu-scroll-down)  ; 滚动
        ("M-h" . corfu-info-documentation) ; 查看文档
        ("M-." . corfu-info-location)      ; 跳转到定义
        ("C-g" . corfu-quit)     ; 退出补全
        ("ESC" . corfu-quit)))   ; ESC 退出

;; Corfu 图标支持
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; 补全后端 - 添加 Tab 补全
(use-package cape
  :ensure t
  :after corfu
  :init
  ;; 添加 Tab 补全功能
  (defun my-setup-cape ()
    "Setup cape completion for non-org modes."
    ;; 只在非 org-mode 中启用补全
    (when (not (derived-mode-p 'org-mode))
      ;; 添加各种补全源
      (add-to-list 'completion-at-point-functions #'cape-dabbrev)
      (add-to-list 'completion-at-point-functions #'cape-dict)
      (add-to-list 'completion-at-point-functions #'cape-file)
      (add-to-list 'completion-at-point-functions #'cape-elisp-block)
      (add-to-list 'completion-at-point-functions #'cape-keyword)
      ;; 添加符号补全（很有用）
      (add-to-list 'completion-at-point-functions #'cape-symbol)
      ;; 添加行补全
      (add-to-list 'completion-at-point-functions #'cape-line)
      
      ;; 设置 Tab 键行为
      (setq completion-cycle-threshold 3)  ; 允许循环补全
      
      ;; 绑定 Tab 键到补全（只在非 org-mode）
      (local-set-key (kbd "TAB") #'my-smart-tab-completion)
      (local-set-key (kbd "<tab>") #'my-smart-tab-completion)))  ;; 在所有编程模式中启用
  (add-hook 'prog-mode-hook #'my-setup-cape)
  (add-hook 'text-mode-hook #'my-setup-cape)
  
  ;; 特定语言的额外补全
  :config
  ;; 对于特定模式添加额外补全
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)))
  
  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-python)))
  
  (add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-c)))
  
  (add-hook 'c++-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-cpp))))

;; 添加一个增强的 Tab 补全函数
(defun my-smart-tab-completion ()
  "智能 Tab 补全：优先使用 corfu，没有补全时缩进。"
  (interactive)
  (if (corfu--capf-p)
      (call-interactively #'completion-at-point)
    (indent-for-tab-command)))

;; 绑定智能 Tab 到全局（可选）
;; (global-set-key (kbd "TAB") #'my-smart-tab-completion)

;; 添加补全历史
(use-package savehist
  :init
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'completion-styles-history))

(use-package consult
  :bind
  ("C-s" . consult-line)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; 可选：配置预览键
  ;; (setq consult-preview-key "M-.")
  
  ;; 为 consult 添加 Tab 支持
  (define-key consult-narrow-map (kbd "TAB") 'consult-narrow)
  (define-key consult-narrow-map (kbd "<tab>") 'consult-narrow))

;; 增强 minibuffer 中的 Tab 补全
(defun my-minibuffer-tab-completion ()
  "Minibuffer 中的 Tab 补全。"
  (interactive)
  (if (minibufferp)
      (if (completing-read-p)
          (minibuffer-complete)
        (insert-char ?\t))
    (call-interactively #'my-smart-tab-completion)))

(use-package consult
    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :bind
    ("C-s" . consult-line)
    :hook (completion-list-mode . consult-preview-at-point-mode)
    :init
    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
    :config
    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    ;; (consult-customize
    ;; consult-theme :preview-key '(:debounce 0.2 any)
    ;; consult-ripgrep consult-git-grep consult-grep
    ;; consult-bookmark consult-recent-file consult-xref
    ;; consult--source-bookmark consult--source-file-register
    ;; consult--source-recent-file consult--source-project-recent-file
    ;; :preview-key "M-."
    ;; :preview-key '(:debounce 0.4 any))

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
        ;;;; 1. project.el (the default)
    ;; (setq consult-project-function #'consult--default-project--function)
        ;;;; 2. vc.el (vc-root-dir)
    ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
        ;;;; 3. locate-dominating-file
    ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
        ;;;; 4. projectile.el (projectile-project-root)
    ;;(autoload 'projectile-project-root "projectile")
    ;;(setq consult-project-function (lambda (_) (projectile-project-root)))
        ;;;; 5. No project support
    ;; (setq consult-project-function nil)
    )

(provide 'config-completion)
