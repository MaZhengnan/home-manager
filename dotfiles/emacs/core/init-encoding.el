;; -*- coding: utf-8; lexical-binding: t; -*-

;; ==================== 编码设置 ====================
;; 强制使用 UTF-8 编码，避免中文乱码等问题
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)

;; 设置默认编码
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; 文件编码
(setq-default buffer-file-coding-system 'utf-8)
(setq-default file-name-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)

;; 进程间通信编码
(setq-default process-coding-system-alist '((utf-8 . utf-8)))

;; 剪贴板编码（系统相关）
(when (memq system-type '(cygwin windows-nt ms-dos))
  (set-clipboard-coding-system 'utf-8))

;; 文件系统编码
(setq file-name-coding-system 'utf-8)

;; 强制编码环境变量（针对 shell 和外部进程）
(when (memq system-type '(gnu/linux darwin))
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LC_CTYPE" "en_US.UTF-8"))

;; Windows 特定编码设置
(when (eq system-type 'windows-nt)
  (setenv "LANG" "zh_CN.UTF-8")  ; Windows 可能需要设为中文编码
  (setq w32-unicode-filenames 'utf-8))

;; 防止自动检测编码导致的乱码
(setq auto-coding-functions nil)
(setq auto-coding-alist nil)
(setq auto-coding-regexp-alist nil)

;; 强制重新加载文件时使用 UTF-8
(defun my/force-utf-8-on-revert ()
  "强制在恢复文件时使用 UTF-8 编码"
  (set-buffer-file-coding-system 'utf-8))
(add-hook 'after-revert-hook #'my/force-utf-8-on-revert)

;; 新建文件时自动设置为 UTF-8
(defun my/set-utf-8-for-new-buffer ()
  "为新缓冲区设置 UTF-8 编码"
  (when (not buffer-file-name)
    (set-buffer-file-coding-system 'utf-8)))
(add-hook 'find-file-hook #'my/set-utf-8-for-new-buffer)

(if (eq system-type 'windows-nt)
	(progn
	  (set-selection-coding-system 'utf-16le-dos) ;;
	  )
  (set-selection-coding-system 'utf-8))
(provide 'init-encoding)
