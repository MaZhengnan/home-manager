;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;Doom Modeline A fancy, fast and customizable mode-line.
(use-package doom-modeline
    :custom
    (doom-modeline-height 25) ;; Set modeline height
    :hook (after-init . doom-modeline-mode))


;; Nerd Icons
;; This is an icon set that can be used with dired, ibuffer and other Emacs packages.
;; Don't forget nerd-icons-install-fonts to install the resource fonts.

;; We use nerd-icons because it supports both GUI and TUI unlike all-the-icons.
;; Also Doom modeline requires nerd icons.
(use-package nerd-icons
    :if (display-graphic-p))

(use-package nerd-icons-dired
    :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
    :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;Set catppuccin theme
(use-package catppuccin-theme
    :config
    (setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha
    (catppuccin-reload)) ;; We need to add t to trust this package

 (defun my/setup-fonts ()
  "最简单的跨平台字体设置"
  (interactive)
  
  ;; 英文字体配置
  (let ((english-config
         (cond
          ((eq system-type 'windows-nt) '("JetBrainsMono NF" 120))
          ((eq system-type 'darwin) '("JetBrains Mono" 160))
          ((eq system-type 'gnu/linux) '("JetBrainsMono NF" 180))
          (t '("Monospace" 100)))))
    
    ;; 中文字体配置
    (let ((chinese-config
           (cond
            ((eq system-type 'windows-nt) "Microsoft YaHei")
            ((eq system-type 'darwin) "PingFang SC")
            ((eq system-type 'gnu/linux) "WenQuanYi Micro Hei")
            (t nil))))
      
      ;; 设置英文字体
      (set-face-attribute 'default nil
                          :font (car english-config)
                          :height (cadr english-config))
      
      ;; 设置中文字体
      (when chinese-config
        (set-fontset-font t 'han (font-spec :family chinese-config)))
      
      ;; 显示结果
      (message "英文字体: %s" (car english-config))
      (when chinese-config
        (message "中文字体: %s" chinese-config)))))

(my/setup-fonts)
  ;; 3. 设置行间距，改善中文阅读体验
(setq-default line-spacing 0.15)
;;(set-frame-parameter nil 'alpha 0.93)
;;With Emacs version 29, true transparency has been added.
;; (add-to-list 'default-frame-alist '(alpha-background . 50)) ;; For all new frames henceforth
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'init-appearance)
