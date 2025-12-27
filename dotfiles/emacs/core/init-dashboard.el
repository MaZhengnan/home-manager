;; -*- coding: utf-8; lexical-binding: t; -*-
;; Simplified Dashboard
(use-package dashboard
  :ensure t
  :init
  ;; Basic configuration
  (setq dashboard-banner-logo-title "Welcome to Emacs"
        dashboard-startup-banner 'logo
        dashboard-page-separator "\n"
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-display-icons-p (and (fboundp 'nerd-icons) t)
        dashboard-set-file-icons (when (fboundp 'nerd-icons) 'nerd-icons)
        dashboard-set-heading-icons (when (fboundp 'nerd-icons) 'nerd-icons)
        
        ;; Only keep files, projects, bookmarks
        dashboard-items '((recents  . 10)
                          (projects . 5)
                          (bookmarks . 5))
        
        ;; Custom startup sequence
        dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-items
                                    dashboard-insert-footer)
        
        ;; Project backend
        dashboard-projects-backend 'project-el
        dashboard-path-style 'truncate-middle
        dashboard-path-max-length 60)
  
  ;; Setup startup hook
  (dashboard-setup-startup-hook)
  
  :config
  ;; Custom footer
  (defun my-dashboard-footer ()
    "Custom dashboard footer."
    (dashboard-insert-center
     (propertize "Press r/p/b to navigate, q to quit"
                 'face 'font-lock-comment-face)))
  
  (advice-add #'dashboard-insert-footer :override #'my-dashboard-footer)
  
  ;; Keybindings for dashboard buffer
  (defun my-dashboard-setup-keys ()
    "Setup keybindings for dashboard buffer."
    (when (eq major-mode 'dashboard-mode)
      ;; Make buffer read-only
      (read-only-mode 1)
      (setq buffer-undo-list t)
      
      ;; Evil mode support
      (if (fboundp 'evil-local-set-key)
          (progn
            (evil-local-set-key 'normal "r" 'dashboard-goto-recent-files)
            (evil-local-set-key 'normal "p" 'dashboard-goto-projects)
            (evil-local-set-key 'normal "b" 'dashboard-goto-bookmarks)
            (evil-local-set-key 'normal "q" 
                               (lambda () 
                                 (interactive)
                                 (kill-buffer)
                                 (switch-to-buffer "*scratch*")))
            (evil-local-set-key 'normal "R" 'dashboard-refresh-buffer)
            (evil-local-set-key 'normal "j" 'next-line)
            (evil-local-set-key 'normal "k" 'previous-line)
            (evil-local-set-key 'normal (kbd "RET") 'widget-button-press)
            
            ;; Prevent entering insert mode
            (evil-local-set-key 'insert (kbd "<escape>") 'evil-normal-state)
            (evil-local-set-key 'insert "i" 'evil-normal-state)
            (evil-local-set-key 'insert "a" 'evil-normal-state))
        
        ;; Fallback keybindings
        (local-set-key "r" 'dashboard-goto-recent-files)
        (local-set-key "p" 'dashboard-goto-projects)
        (local-set-key "b" 'dashboard-goto-bookmarks)
        (local-set-key "q" (lambda () (interactive) (kill-buffer)))
        (local-set-key "R" 'dashboard-refresh-buffer))))
  
  (add-hook 'dashboard-mode-hook #'my-dashboard-setup-keys)
  
  ;; Ensure only dashboard is shown
  (defun my-open-dashboard ()
    "Open dashboard in full window."
    (interactive)
    (delete-other-windows)
    (dashboard-refresh-buffer)
    (dashboard-goto-recent-files))
  
  (advice-add 'dashboard-open :override #'my-open-dashboard))

;; Global key to open dashboard
(global-set-key (kbd "C-c d") 'dashboard-open)

(provide 'init-dashboard)