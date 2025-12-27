;; -*- coding: utf-8; lexical-binding: t; -*-
;;; evil-keybindings.el
;;; Commentary:
;;; Code:

;;; -*- lexical-binding: t; -*-

(use-package evil
    :init
    (evil-mode)
    :config
    (evil-set-initial-state 'eat-mode 'insert) ;; Set initial state in eat terminal to insert mode
    :custom
    (evil-want-keybinding nil)    ;; Disable evil bindings in other modes (It's not consistent and not good)
    (evil-want-C-u-scroll t)      ;; Set C-u to scroll up
    (evil-want-C-i-jump nil)      ;; Disables C-i jump
    (evil-undo-system 'undo-redo) ;; C-r to redo
    ;; Unmap keys in 'evil-maps. If not done, org-return-follows-link will not work
    :bind (:map evil-motion-state-map
                ("SPC" . nil)
                ("RET" . nil)
                ("TAB" . nil)))

(use-package evil-collection
    :after evil
    :config
    ;; Setting where to use evil-collection
    (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult info))
    (evil-collection-init))

(use-package evil-escape
    :after evil
    :ensure t
    :config
    (setq-default evil-escape-key-sequence "jk") ;; Push "jk" back normal from insert
    (setq evil-escape-delay 0.5) ;; delay time is 0.5s
    (evil-escape-mode 1))

(use-package general
  :config
  ;; (general-evil-setup) ;; <- evil
  ;; Set up 'C-SPC' as the leader key
  (general-create-definer start/leader-keys
    :states '(normal insert visual motion emacs) ;; <- evil
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC") ;; Set global leader key so we can access our keybindings from any state

  (start/leader-keys
    "." '(find-file :wk "Find file")
    "TAB" '(comment-line :wk "Comment lines")
    "SPC" '(execute-extended-command :wk "M-x"))

  (start/leader-keys
    "b" '(:ignore t :wk "Buffers")
    "b b" '(consult-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers"))

  (start/leader-keys
    "c" '(:ignore t :wk "Code")
    "c b" '(my/cmake-build :wk "Build C/Cpp")
    "c c" '(:ignore t :wk "CMake")
    "c c c" '(my/cmake-configure :wk "CMake Configure")
    "c c C" '(my/cmake-clean :wk "CMake clean")
    "c d" '(kill-some-buffers :wk "Debug")
    "c f" '(flymake-show-buffer-diagnostics :wk "Flymake buffer diagnostic")
    "c r" '(my/cmake-run :wk "Run C/CPP")
    "c p" '(previous-buffer :wk "Run python"))

  (start/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d v" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current"))

  (start/leader-keys
    "e" '(:ignore t :wk "Languages")
    "e e" '(eglot-reconnect :wk "Eglot Reconnect")
    "e d" '(eldoc-doc-buffer :wk "Eldoc Buffer")
    "e f" '(eglot-format :wk "Eglot Format")
    "e l" '(consult-flymake :wk "Consult Flymake")
    "e r" '(eglot-rename :wk "Eglot Rename")
    "e i" '(xref-find-definitions :wk "Find definition")
    "e v" '(:ignore t :wk "Elisp")
    "e v b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e v r" '(eval-region :wk "Evaluate elisp in region"))

  (start/leader-keys
    "f" '(:ignore t :wk "Files")
    "f c" '((lambda () (interactive)
              (find-file "~/.emacs.d/init.org"))
            :wk "Open emacs init.org")
    "f e" '((lambda () (interactive)
              (dired "~/.emacs.d"))
            :wk "Open user-emacs-directory in dired")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
    "f l" '(counsel-locate :wk "Locate a file")
    "f r" '(consult-recent-file :wk "Find recent files"))

  (start/leader-keys
    "g" '(:ignore t :wk "Git")
    "g s" '(magit-status :wk "Magit status"))

  (start/leader-keys
    "h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
    "h c" '((lambda () (interactive)
              (load-file "~/.emacs.d/init.el"))
            :wk "Reload Emacs config")
    "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "h r"  '(restart-emacs :wk "Restart Emacs"))

  (start/leader-keys
	"p" `(,project-prefix-map :wk "Project"))

  (start/leader-keys
    "s" '(:ignore t :wk "Search")
    "s c" '((lambda () (interactive) (find-file "~/.config/emacs/init.org")) :wk "Find emacs Config")
    "s r" '(consult-recent-file :wk "Search recent files")
    "s f" '(consult-fd :wk "Search files with fd")
    "s g" '(consult-ripgrep :wk "Search with ripgrep")
    "s l" '(consult-line :wk "Search line")
    "s i" '(consult-imenu :wk "Search Imenu buffer locations")) ;; This one is really cool

  (start/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t f" '(flymake-mode :wk "Toggle flymake mode")
    "t l" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
    "t t" '(eat :wk "Eat terminal"))

  (start/leader-keys
    "w" '(:ignore t :wk "Windows/Words")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right")
    ;; Words
    "w d" '(downcase-word :wk "Downcase word")
    "w u" '(upcase-word :wk "Upcase word")
    "w =" '(count-words :wk "Count words/lines for buffer")))

 (use-package which-key
           :ensure nil ;; Don't install which-key because it's now built-in
           :init
           (which-key-mode 1)
           :diminish
           :custom
           (which-key-side-window-location 'bottom)
           (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
           (which-key-sort-uppercase-first nil)
           (which-key-add-column-padding 1) ;; Number of spaces to add to the left of each column
           (which-key-min-display-lines 6)  ;; Increase the minimum lines to display because the default is only 1
           (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
           (which-key-max-description-length 25)
           (which-key-separator " → " )
           (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

(provide 'config-evil)
