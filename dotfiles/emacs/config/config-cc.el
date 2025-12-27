;; -*- coding: utf-8; lexical-binding: t; -*-
;;; config-cc.el 
;;; Commentary:
;;; Code:

(use-package cmake-mode
   :ensure t
   :mode (("CMakeLists\\.txt\\'" . cmake-mode)
          ("\\.cmake\\'" . cmake-mode)))





(provide 'config-cc)
