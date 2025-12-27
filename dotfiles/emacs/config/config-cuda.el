;; -*- coding: utf-8; lexical-binding: t; -*-
;;; config-cuda.el 
;;; Commentary:
;;; Code:

(use-package cuda-mode
  :ensure t
  :mode ("\\.cu\\'" "\\.cuh\\'")
  :config
  ;; 可选：与 tree-sitter 结合（如果可用）
  (when (fboundp 'c-ts-mode)
    (add-hook 'cuda-mode-hook 'c-ts-mode)))

;; Emacs 配置
(add-hook 'cuda-mode-hook 'eglot-ensure)
(provide 'config-cuda)
