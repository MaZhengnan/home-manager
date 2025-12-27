;; -*- coding: utf-8; lexical-binding: t; -*-
;;; config-ssh.el 
;;; Commentary:
;;; Code:

;;; wsl.el --- WSL integration (fixed UTF-16 output) -*- lexical-binding: t; -*-

(require 'tramp)
(require 'subr-x)

(require 'tramp)
(require 'subr-x)

;; ====== 核心：获取 WSL 发行版列表 ======
(defun my/windows-command-utf16le (command)
  "运行 Windows 命令并解码 UTF-16LE 输出"
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-16le)
          (coding-system-for-write 'utf-16le))
      (call-process-shell-command command nil t nil))
    (string-trim (buffer-string))))

(defun my/wsl-list-distros ()
  "返回已安装的 WSL 发行版列表"
  (let* ((output (my/windows-command-utf16le "wsl.exe -l -q"))
         (lines (split-string output "\r?\n" t)))
    ;; 清理输出：去除空行和可能的多余字符
    (delq nil (mapcar (lambda (line)
                        (when (and (> (length line) 0)
                                   (not (string-match-p "^\s*$" line)))
                          (string-trim line)))
                      lines))))

;; ====== 核心：获取 WSL 的 SSH 连接信息 ======
(defun my/get-wsl-ssh-info (distro)
  "获取 WSL 发行版的 SSH 连接信息（用户名和IP）"
  (let* ((ip-command (format "wsl.exe -d %s -- ip -4 addr show eth0 | grep inet | awk '{print $2}' | cut -d/ -f1" distro))
         (ip (string-trim (shell-command-to-string ip-command)))
         (user-command (format "wsl.exe -d %s -- whoami" distro))
         (user (string-trim (shell-command-to-string user-command))))
    (cons user ip)))

;; ====== 主函数：智能连接（合并 WSL 和 SSH）=====
(defun my/connect-remote ()
  "智能连接远程服务器：支持 WSL 和普通 SSH"
  (interactive)
  (let* ((choices '(("WSL (通过SSH)" . :wsl-ssh)
                    ("普通 SSH 服务器" . :ssh)
                    ("SFTP 服务器" . :sftp)
                    ("Docker 容器" . :docker)))
         (action (cdr (assoc (completing-read "连接方式: " 
                                              (mapcar 'car choices) nil t)
                             choices)))
         path)
    
    (cond
     ;; ====== WSL 通过 SSH 连接 ======
     ((eq action :wsl-ssh)
      (let* ((distros (my/wsl-list-distros))
             (distro (completing-read "选择 WSL 发行版: " distros nil t))
             (ssh-info (my/get-wsl-ssh-info distro))
             (user (car ssh-info))
             (ip (cdr ssh-info))
             (remote-path (read-string "远程路径: " nil nil "~/")))
        
        (if (and user ip)
            (setq path (format "/ssh:%s@%s:%s" user ip remote-path))
          (message "无法获取 WSL 的 SSH 信息，尝试直接连接")
          (setq path (format "/wsl:%s:%s" distro remote-path)))))
     
     ;; ====== 普通 SSH 连接 ======
     ((eq action :ssh)
      (let ((host (read-string "主机名或 IP: "))
            (user (read-string "用户名: " nil nil (user-login-name)))
            (remote-path (read-string "远程路径: " nil nil "~/")))
        (setq path (format "/ssh:%s@%s:%s" user host remote-path))))
     
     ;; ====== SFTP 连接 ======
     ((eq action :sftp)
      (let ((host (read-string "主机名或 IP: "))
            (user (read-string "用户名: "))
            (remote-path (read-string "远程路径: ")))
        (setq path (format "/sftp:%s@%s:%s" user host remote-path))))
     
     ;; ====== Docker 连接 ======
     ((eq action :docker)
      (let ((container (read-string "容器名或 ID: "))
            (remote-path (read-string "容器内路径: " nil nil "/")))
        (setq path (format "/docker:%s:%s" container remote-path)))))
    
    (when path
      (find-file path))))

;; ====== 专用函数：快速连接 WSL ======
(defun my/connect-wsl-quick ()
  "快速连接到 WSL（自动获取信息）"
  (interactive)
  (let* ((distros (my/wsl-list-distros))
         (distro (if (= (length distros) 1)
                     (car distros)
                   (completing-read "选择 WSL 发行版: " distros nil t)))
         (ssh-info (my/get-wsl-ssh-info distro))
         (user (car ssh-info))
         (ip (cdr ssh-info)))
    
    (if (and user ip)
        (find-file (format "/ssh:%s@%s:~/"))
      ;; 如果无法获取 SSH 信息，尝试直接连接
      (find-file (format "/wsl:%s:~/")))))

;; ====== 专用函数：快速连接常用 WSL ======
(defun my/connect-wsl-rocky ()
  "快速连接到 Rocky Linux WSL"
  (interactive)
  (let ((ssh-info (my/get-wsl-ssh-info "rocky")))
    (if (cdr ssh-info)  ; 如果有 IP
        (find-file (format "/ssh:%s@%s:~/"))
      (find-file "/wsl:rocky:~/"))))

;; ====== SSH 配置集成 ======
(defun my/ssh-connect-with-config ()
  "使用 ~/.ssh/config 中的配置连接"
  (interactive)
  (let* ((ssh-config-file (expand-file-name "~/.ssh/config"))
         hosts)
    (when (file-exists-p ssh-config-file)
      (with-temp-buffer
        (insert-file-contents ssh-config-file)
        (goto-char (point-min))
        (while (re-search-forward "^Host\\s-+\\(.+\\)" nil t)
          (let ((host-line (match-string 1)))
            (dolist (host (split-string host-line))
              (unless (or (string= host "*") 
                          (member host hosts))
                (push host hosts))))))
      
      (let* ((selected (completing-read "选择 SSH 主机: " (nreverse hosts) nil t))
             (remote-path (read-string "远程路径: " nil nil "~/")))
        (find-file (format "/ssh:%s:%s" selected remote-path)))
      
      (error "~/.ssh/config 文件不存在"))))

;; ====== 预设连接管理器 ======
(defvar my/remote-connections
  '(("Rocky Linux (WSL)" . :wsl-rocky)
    ("Ubuntu (WSL)" . :wsl-ubuntu)
    ("我的服务器" . "/ssh:user@server.com:~/")
    ("Docker 测试" . "/docker:ubuntu:/"))
  "预设连接列表")

(defun my/quick-connect ()
  "从预设列表快速连接"
  (interactive)
  (let* ((selected (completing-read "快速连接: " 
                                   (mapcar 'car my/remote-connections) nil t))
         (target (cdr (assoc selected my/remote-connections))))
    
    (cond
     ((eq target :wsl-rocky)
      (my/connect-wsl-rocky))
     ((eq target :wsl-ubuntu)
      (my/connect-wsl-quick))  ; 让用户选择发行版
     ((stringp target)
      (find-file target)))))

;; ====== 键盘快捷键 ======
(global-set-key (kbd "C-c W") 'my/connect-remote)        ; 智能连接（主函数）
(global-set-key (kbd "C-c w") 'my/connect-wsl-quick)     ; 快速连接 WSL
(global-set-key (kbd "C-c r") 'my/connect-wsl-rocky)     ; 快速连接 Rocky
(global-set-key (kbd "C-c S") 'my/ssh-connect-with-config) ; SSH 配置连接
(global-set-key (kbd "C-c Q") 'my/quick-connect)         ; 预设连接

;; ====== TRAMP 性能优化 ======
(setq tramp-default-method "ssh")
(setq tramp-use-ssh-controlmaster-options t)
(setq tramp-verbose 0)
(setq tramp-connection-timeout 10)

;; 启用 SSH 配置补全
(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "~/.ssh/config")))

(provide 'config-ssh)
