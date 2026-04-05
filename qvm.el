;;; qvm.el --- QEMU VM manager for Emacs -*- lexical-binding: t; -*-

;; Author: James Dyer
;; Version: 1.0.0
;; Keywords: tools, qemu, vm
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))

;;; Commentary:

;; Manages QEMU virtual machines directly from Emacs.
;; Calls qemu-system-x86_64, qemu-img, vncviewer, spicy, and ssh
;; directly -- no external wrapper script required.
;;
;; Features:
;;   - VM list buffer (qvm-list) with keybindings for common operations
;;   - Transient menu (?) for discoverable access to all commands
;;   - TRAMP integration: open shell or dired on a running VM
;;   - Start, stop, run (start+viewer), VNC and SPICE commands
;;   - Async command output shown in a dedicated buffer
;;
;; Usage:
;;   M-x qvm-list     -- open the VM manager buffer
;;   M-x qvm-menu     -- open the transient menu (also ? in list buffer)
;;
;; Keybindings in the VM list buffer:
;;   RET / r  -- run (start + viewer)
;;   s        -- start
;;   x        -- stop (with confirmation)
;;   c        -- create new VM (prompts for name, ISO, disk/mem/cpus)
;;   v        -- open VNC viewer
;;   V        -- open SPICE viewer
;;   d        -- open dired via TRAMP
;;   e        -- open eshell via TRAMP
;;   S        -- rsync files to VM
;;   P        -- push SSH key to VM (passwordless access)
;;   C        -- clone VM
;;   +        -- create snapshot
;;   N        -- list snapshots
;;   R        -- restore snapshot
;;   X        -- delete snapshot
;;   i        -- show VM info
;;   g        -- refresh
;;   q        -- quit
;;   ?        -- transient menu
;;
;; Add to your init.el:
;;   (use-package qvm
;;     :load-path "~/source/repos/qvm"
;;     :bind ("C-c v" . qvm-list))

;;; Code:

(require 'tabulated-list)
(require 'transient)
(require 'tramp)

;; ── Customization ────────────────────────────────────────────────────────────

(defgroup qvm nil
  "QEMU VM manager."
  :group 'tools
  :prefix "qvm-")

(defcustom qvm-base-dir (expand-file-name "~/VM")
  "Directory where VMs are stored."
  :type 'directory
  :group 'qvm)

(defcustom qvm-qemu-binary (or (executable-find "qemu-system-x86_64")
                                "qemu-system-x86_64")
  "Path to the QEMU system emulator."
  :type 'file
  :group 'qvm)

(defcustom qvm-vnc-viewer (or (executable-find "vncviewer") "vncviewer")
  "Path to the VNC viewer program."
  :type 'file
  :group 'qvm)

(defcustom qvm-vnc-viewer-args '("-RemoteResize=1")
  "Extra arguments passed to the VNC viewer."
  :type '(repeat string)
  :group 'qvm)

(defcustom qvm-spice-viewer (or (executable-find "spicy") "spicy")
  "Path to the SPICE viewer program."
  :type 'file
  :group 'qvm)

(defcustom qvm-default-memory "4G"
  "Default memory size for new VMs."
  :type 'string
  :group 'qvm)

(defcustom qvm-default-cpus "4"
  "Default CPU count for new VMs."
  :type 'string
  :group 'qvm)

(defcustom qvm-default-disk "40G"
  "Default disk size for new VMs."
  :type 'string
  :group 'qvm)

(defcustom qvm-default-ssh-port 2222
  "Starting SSH port for auto-assignment."
  :type 'integer
  :group 'qvm)

(defcustom qvm-default-vnc-display 1
  "Starting VNC display number for auto-assignment."
  :type 'integer
  :group 'qvm)

(defcustom qvm-default-spice-port 5930
  "Starting SPICE port for auto-assignment."
  :type 'integer
  :group 'qvm)

(defcustom qvm-default-display "vnc"
  "Default display type for new VMs."
  :type '(choice (const "vnc") (const "spice"))
  :group 'qvm)

(defcustom qvm-default-user (user-login-name)
  "Default SSH username for new VMs."
  :type 'string
  :group 'qvm)

(defcustom qvm-output-buffer "*qvm output*"
  "Buffer name for command output."
  :type 'string
  :group 'qvm)

;; ── Path helpers ─────────────────────────────────────────────────────────────

(defun qvm--vm-dir (name)
  "Return the directory path for VM NAME."
  (expand-file-name name qvm-base-dir))

(defun qvm--vm-conf (name)
  "Return the config file path for VM NAME."
  (expand-file-name "vm.conf" (qvm--vm-dir name)))

(defun qvm--vm-disk (name)
  "Return the disk image path for VM NAME."
  (expand-file-name "disk.qcow2" (qvm--vm-dir name)))

(defun qvm--vm-pid (name)
  "Return the PID file path for VM NAME."
  (expand-file-name "qvm.pid" (qvm--vm-dir name)))

;; ── Config parsing ───────────────────────────────────────────────────────────

(defun qvm--list-vms ()
  "Return a list of VM names found in `qvm-base-dir'."
  (when (file-directory-p qvm-base-dir)
    (seq-filter
     (lambda (name)
       (file-exists-p (qvm--vm-conf name)))
     (directory-files qvm-base-dir nil "^[^.]"))))

(defun qvm--read-conf (name)
  "Read vm.conf for VM NAME and return an alist of key/value pairs."
  (let ((conf-file (qvm--vm-conf name))
        result)
    (when (file-exists-p conf-file)
      (with-temp-buffer
        (insert-file-contents conf-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([A-Z_]+\\)=\"\\(.*\\)\"" nil t)
          (push (cons (match-string 1) (match-string 2)) result))))
    result))

(defun qvm--conf-get (conf key)
  "Get KEY from parsed conf alist CONF."
  (cdr (assoc key conf)))

(defun qvm--write-conf (name conf)
  "Write CONF alist to vm.conf for VM NAME."
  (with-temp-file (qvm--vm-conf name)
    (insert (format "# qvm configuration for '%s'\n" name))
    (insert (format "# Generated %s\n" (format-time-string "%FT%T%z")))
    (dolist (pair conf)
      (insert (format "%s=\"%s\"\n" (car pair) (cdr pair))))))

(defun qvm--update-conf-value (name key value)
  "Update KEY to VALUE in vm.conf for VM NAME."
  (let ((conf-file (qvm--vm-conf name)))
    (with-temp-buffer
      (insert-file-contents conf-file)
      (goto-char (point-min))
      (if (re-search-forward (format "^%s=\".*\"" (regexp-quote key)) nil t)
          (replace-match (format "%s=\"%s\"" key value))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "%s=\"%s\"\n" key value)))
      (write-region (point-min) (point-max) conf-file nil 'silent))))

;; ── State queries ────────────────────────────────────────────────────────────

(defun qvm--pid-alive-p (pid)
  "Return non-nil if process PID is alive."
  (= 0 (call-process "kill" nil nil nil "-0" (number-to-string pid))))

(defun qvm--read-pid (name)
  "Read and return the PID from VM NAME's pidfile, or nil."
  (let ((pidfile (qvm--vm-pid name)))
    (when (file-exists-p pidfile)
      (let ((pid (string-to-number
                  (string-trim (with-temp-buffer
                                 (insert-file-contents pidfile)
                                 (buffer-string))))))
        (when (> pid 0) pid)))))

(defun qvm--running-p (name)
  "Return non-nil if VM NAME is currently running."
  (when-let ((pid (qvm--read-pid name)))
    (qvm--pid-alive-p pid)))

(defun qvm--disk-size (name)
  "Return human-readable disk usage for VM NAME."
  (let ((disk (qvm--vm-disk name)))
    (if (file-exists-p disk)
        (string-trim
         (shell-command-to-string
          (format "du -sh %s 2>/dev/null | cut -f1" (shell-quote-argument disk))))
      "?")))

(defun qvm--ssh-config-host-p (name)
  "Return non-nil if SSH config has a Host entry for qvm-NAME."
  (let ((ssh-config (expand-file-name "~/.ssh/config")))
    (when (file-exists-p ssh-config)
      (with-temp-buffer
        (insert-file-contents ssh-config)
        (goto-char (point-min))
        (re-search-forward (format "^Host qvm-%s$" (regexp-quote name)) nil t)))))

(defun qvm--tramp-path (name &optional path)
  "Return a TRAMP path to VM NAME at remote PATH (default /home/user/).
Uses the SSH config alias qvm-NAME if available (set up by `qvm-ssh-copy-id'),
otherwise falls back to /ssh:user@localhost#port:path."
  (let* ((conf (qvm--read-conf name))
         (user (qvm--conf-get conf "VM_USER"))
         (port (qvm--conf-get conf "VM_SSH_PORT"))
         (remote-path (or path (format "/home/%s/" user))))
    (if (qvm--ssh-config-host-p name)
        (format "/ssh:qvm-%s:%s" name remote-path)
      (format "/ssh:%s@localhost#%s:%s" user port remote-path))))

(defun qvm--snapshot-tags (name)
  "Return a list of snapshot tag names for VM NAME."
  (let ((disk (qvm--vm-disk name)))
    (when (file-exists-p disk)
      (let ((output (shell-command-to-string
                     (format "qemu-img snapshot -l %s 2>/dev/null"
                             (shell-quote-argument disk)))))
        (let (tags)
          (dolist (line (split-string output "\n" t))
            (when (string-match "^[0-9]+\\s-+\\(\\S-+\\)" line)
              (push (match-string 1 line) tags)))
          (nreverse tags))))))

;; ── Port allocation ──────────────────────────────────────────────────────────

(defun qvm--port-in-use-p (port)
  "Return non-nil if TCP PORT is in use."
  (with-temp-buffer
    (call-process "ss" nil t nil "-tlnH" (format "sport = :%d" port))
    (> (buffer-size) 0)))

(defun qvm--next-free-port (start-port)
  "Find next free TCP port starting from START-PORT."
  (let ((port start-port))
    (while (qvm--port-in-use-p port)
      (setq port (1+ port)))
    port))

(defun qvm--next-free-vnc (start-display)
  "Find next free VNC display starting from START-DISPLAY.
VNC display N maps to TCP port 5900+N."
  (let ((display start-display))
    (while (qvm--port-in-use-p (+ 5900 display))
      (setq display (1+ display)))
    display))

;; ── Process helpers ──────────────────────────────────────────────────────────

(defun qvm--run-process (proc-name program args &optional on-finish)
  "Run PROGRAM with ARGS asynchronously, showing output in `qvm-output-buffer'.
PROC-NAME names the process.  ON-FINISH called on successful exit."
  (let ((buf (get-buffer-create qvm-output-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "$ %s %s\n\n" program (string-join args " ")))
      (setq buffer-read-only t))
    (display-buffer buf '(display-buffer-at-bottom . ((window-height . 10))))
    (make-process
     :name proc-name
     :buffer buf
     :command (cons program args)
     :filter (lambda (proc str)
               (when (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc)
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert str)))))
     :sentinel (lambda (proc event)
                 (when (buffer-live-p (process-buffer proc))
                   (with-current-buffer (process-buffer proc)
                     (let ((inhibit-read-only t))
                       (goto-char (point-max))
                       (insert (format "\n[%s]" (string-trim event))))))
                 (when (and on-finish (string-match-p "finished" event))
                   (funcall on-finish))))))

(defun qvm--display-output (text)
  "Display TEXT in the qvm output buffer."
  (let ((buf (get-buffer-create qvm-output-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert text)
      (setq buffer-read-only t))
    (display-buffer buf '(display-buffer-at-bottom . ((window-height . 10))))))

(defun qvm--append-output (text)
  "Append TEXT to the qvm output buffer."
  (when-let ((buf (get-buffer qvm-output-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))))

;; ── QEMU helpers ─────────────────────────────────────────────────────────────

(defun qvm--qemu-display-args (conf)
  "Return QEMU display argument list based on CONF."
  (let ((display (or (qvm--conf-get conf "VM_DISPLAY") "vnc")))
    (if (string= display "spice")
        (let ((port (or (qvm--conf-get conf "VM_SPICE_PORT")
                        (number-to-string qvm-default-spice-port))))
          (list "-spice" (format "port=%s,disable-ticketing=on" port)
                "-device" "virtio-serial-pci"
                "-chardev" "spicevmc,id=vdagent,name=vdagent"
                "-device" "virtserialport,chardev=vdagent,name=com.redhat.spice.0"
                "-display" "none"
                "-vga" "qxl"))
      (let ((vnc-display (or (qvm--conf-get conf "VM_VNC_DISPLAY")
                             (number-to-string qvm-default-vnc-display))))
        (list "-vnc" (format ":%s" vnc-display)
              "-display" "none"
              "-vga" "virtio")))))

(defun qvm--qemu-base-args (name conf &optional offline)
  "Build base QEMU command args for VM NAME using CONF.
If OFFLINE is non-nil, disable networking."
  (let* ((disk (qvm--vm-disk name))
         (memory (or (qvm--conf-get conf "VM_MEMORY") qvm-default-memory))
         (cpus (or (qvm--conf-get conf "VM_CPUS") qvm-default-cpus))
         (ssh-port (or (qvm--conf-get conf "VM_SSH_PORT")
                       (number-to-string qvm-default-ssh-port))))
    (append
     (list "-enable-kvm"
           "-m" memory
           "-smp" cpus
           "-cpu" "host"
           "-drive" (format "file=%s,format=qcow2" disk))
     (if offline
         (list "-nic" "none")
       (list "-nic" (format "user,hostfwd=tcp::%s-:22" ssh-port)))
     (qvm--qemu-display-args conf))))

(defun qvm--start-qemu (name args &optional on-exit)
  "Launch QEMU for VM NAME with ARGS.
ON-EXIT is called when the QEMU process terminates.
Returns the process object."
  (unless (executable-find qvm-qemu-binary)
    (user-error "QEMU binary not found: %s" qvm-qemu-binary))
  (let* ((pidfile (qvm--vm-pid name))
         (proc (make-process
                :name (format "qemu-%s" name)
                :command (cons qvm-qemu-binary args)
                :sentinel (lambda (_proc event)
                            (when (file-exists-p pidfile)
                              (delete-file pidfile))
                            (when (get-buffer "*qvm*")
                              (qvm-list-refresh))
                            (when (and on-exit
                                       (string-match-p "\\(?:finished\\|exited\\)" event))
                              (funcall on-exit))))))
    (set-process-query-on-exit-flag proc nil)
    (with-temp-file pidfile
      (insert (number-to-string (process-id proc))))
    proc))

;; ── SSH helpers ──────────────────────────────────────────────────────────────

(defun qvm--ssh-args (name)
  "Return base SSH argument list for connecting to VM NAME."
  (if (qvm--ssh-config-host-p name)
      (list (format "qvm-%s" name))
    (let* ((conf (qvm--read-conf name))
           (user (qvm--conf-get conf "VM_USER"))
           (port (qvm--conf-get conf "VM_SSH_PORT")))
      (list "-p" port "-o" "StrictHostKeyChecking=accept-new"
            (format "%s@localhost" user)))))

(defun qvm--add-ssh-config (name user port)
  "Add SSH config entry for VM NAME if not already present."
  (let* ((ssh-config (expand-file-name "~/.ssh/config"))
         (host-alias (format "qvm-%s" name)))
    (unless (and (file-exists-p ssh-config)
                 (with-temp-buffer
                   (insert-file-contents ssh-config)
                   (goto-char (point-min))
                   (re-search-forward
                    (format "^Host %s$" (regexp-quote host-alias)) nil t)))
      (make-directory (expand-file-name "~/.ssh") t)
      (with-temp-buffer
        (insert (format "\n# qvm: %s\nHost %s\n    HostName localhost\n    Port %s\n    User %s\n    BatchMode yes\n    StrictHostKeyChecking accept-new\n    LogLevel ERROR\n"
                        name host-alias port user))
        (append-to-file (point-min) (point-max) ssh-config))
      (set-file-modes ssh-config #o600)
      (message "Added SSH config entry '%s'" host-alias))))

;; ── Commands ─────────────────────────────────────────────────────────────────

;;;###autoload
(defun qvm-start (name)
  "Start VM NAME."
  (interactive (list (completing-read "Start VM: " (qvm--list-vms) nil t)))
  (when (qvm--running-p name)
    (user-error "VM '%s' is already running" name))
  (let* ((conf (qvm--read-conf name))
         (args (qvm--qemu-base-args name conf)))
    (qvm--start-qemu name args)
    (qvm--display-output
     (format "Starting VM '%s'\n  Memory:  %s\n  CPUs:    %s\n  SSH:     localhost:%s\n  Display: %s\n"
             name
             (or (qvm--conf-get conf "VM_MEMORY") "?")
             (or (qvm--conf-get conf "VM_CPUS") "?")
             (or (qvm--conf-get conf "VM_SSH_PORT") "?")
             (or (qvm--conf-get conf "VM_DISPLAY") "vnc")))
    (qvm-list-refresh)))

;;;###autoload
(defun qvm-stop (name)
  "Stop VM NAME."
  (interactive (list (completing-read "Stop VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (let ((pid (qvm--read-pid name)))
    (unless pid
      (user-error "VM '%s' does not appear to be running" name))
    (if (qvm--pid-alive-p pid)
        (progn
          (qvm--display-output (format "Stopping VM '%s' (PID %d)...\n" name pid))
          (signal-process pid 15)
          (qvm--wait-for-stop name pid 0))
      (let ((pidfile (qvm--vm-pid name)))
        (when (file-exists-p pidfile)
          (delete-file pidfile)))
      (message "Removed stale PID file for '%s'" name)
      (qvm-list-refresh))))

(defun qvm--wait-for-stop (name pid attempts)
  "Poll for PID to die after stopping VM NAME.
Force-kill after 10 ATTEMPTS."
  (cond
   ((not (qvm--pid-alive-p pid))
    (let ((pidfile (qvm--vm-pid name)))
      (when (file-exists-p pidfile)
        (delete-file pidfile)))
    (qvm--append-output "Stopped.\n")
    (qvm-list-refresh))
   ((>= attempts 10)
    (signal-process pid 9)
    (let ((pidfile (qvm--vm-pid name)))
      (when (file-exists-p pidfile)
        (delete-file pidfile)))
    (qvm--append-output "Force stopped.\n")
    (qvm-list-refresh))
   (t
    (run-with-timer 1 nil #'qvm--wait-for-stop name pid (1+ attempts)))))

;;;###autoload
(defun qvm-run (name)
  "Start VM NAME and open the display viewer."
  (interactive (list (completing-read "Run VM: " (qvm--list-vms) nil t)))
  (let* ((conf (qvm--read-conf name))
         (display (or (qvm--conf-get conf "VM_DISPLAY") "vnc")))
    (unless (qvm--running-p name)
      (qvm-start name))
    (run-with-timer 1 nil
                    (lambda ()
                      (if (string= display "spice")
                          (qvm-spice name)
                        (qvm-vnc name))))))

;;;###autoload
(defun qvm-vnc (name)
  "Open VNC viewer for VM NAME."
  (interactive (list (completing-read "VNC to VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (unless (qvm--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((conf (qvm--read-conf name))
         (vnc-display (qvm--conf-get conf "VM_VNC_DISPLAY"))
         (vnc-port (+ 5900 (string-to-number (or vnc-display "1")))))
    (qvm--run-process "qvm-vnc" qvm-vnc-viewer
                      (append (list (format "localhost:%d" vnc-port))
                              qvm-vnc-viewer-args))))

;;;###autoload
(defun qvm-spice (name)
  "Open SPICE viewer for VM NAME."
  (interactive (list (completing-read "SPICE to VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (unless (qvm--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((conf (qvm--read-conf name))
         (spice-port (or (qvm--conf-get conf "VM_SPICE_PORT")
                         (number-to-string qvm-default-spice-port))))
    (qvm--run-process "qvm-spice" qvm-spice-viewer
                      (list "-h" "localhost" "-p" spice-port))))

;;;###autoload
(defun qvm-display (name)
  "Toggle display type between vnc and spice for VM NAME."
  (interactive (list (completing-read "Toggle display for VM: " (qvm--list-vms) nil t)))
  (when (qvm--running-p name)
    (user-error "VM '%s' is running -- stop it first to switch display" name))
  (let* ((conf (qvm--read-conf name))
         (current (or (qvm--conf-get conf "VM_DISPLAY") "vnc"))
         (new (if (string= current "vnc") "spice" "vnc")))
    (qvm--update-conf-value name "VM_DISPLAY" new)
    (message "Switched '%s' to %s (takes effect on next start)" name new)
    (qvm-list-refresh)))

;;;###autoload
(defun qvm-keyboard (name)
  "Setup keyboard remaps and sticky keys on VM NAME via SSH."
  (interactive (list (completing-read "Keyboard setup for VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (unless (qvm--running-p name)
    (user-error "VM '%s' is not running" name))
  (let ((ssh-args (qvm--ssh-args name))
        (remote-cmd (string-join
                     '("gsettings set org.gnome.desktop.input-sources sources \"[('xkb', 'gb')]\""
                       "gsettings set org.gnome.desktop.input-sources xkb-options \"['ctrl:nocaps','ctrl:ralt_rctrl']\""
                       "gsettings set org.gnome.desktop.a11y.keyboard stickykeys-enable true")
                     " && ")))
    (qvm--run-process "qvm-keyboard" "ssh"
                      (append ssh-args (list remote-cmd)))))

;;;###autoload
(defun qvm-clip-copy (name)
  "Copy VM NAME clipboard to host clipboard (kill ring)."
  (interactive (list (completing-read "Copy clipboard from VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (unless (qvm--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((ssh-args (qvm--ssh-args name))
         (remote-cmd (concat
                      "if command -v wl-paste >/dev/null 2>&1; then wl-paste;"
                      " elif command -v xclip >/dev/null 2>&1; then xclip -selection clipboard -o;"
                      " elif command -v xsel >/dev/null 2>&1; then xsel --clipboard --output;"
                      " else echo 'No clipboard tool found' >&2; exit 1; fi"))
         (content (with-temp-buffer
                    (let ((exit-code (apply #'call-process "ssh" nil t nil
                                           (append ssh-args (list remote-cmd)))))
                      (unless (= exit-code 0)
                        (user-error "Failed to read VM clipboard"))
                      (buffer-string)))))
    (kill-new content)
    (with-temp-buffer
      (insert content)
      (call-process-region (point-min) (point-max) "wl-copy"))
    (message "Copied VM clipboard -> host (%d chars)" (length content))))

;;;###autoload
(defun qvm-clip-paste (name)
  "Paste host clipboard to VM NAME clipboard."
  (interactive (list (completing-read "Paste clipboard to VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (unless (qvm--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((ssh-args (qvm--ssh-args name))
         (content (with-temp-buffer
                    (call-process "wl-paste" nil t nil)
                    (buffer-string)))
         (remote-cmd (concat
                      "if command -v wl-copy >/dev/null 2>&1; then wl-copy;"
                      " elif command -v xclip >/dev/null 2>&1; then xclip -selection clipboard;"
                      " elif command -v xsel >/dev/null 2>&1; then xsel --clipboard --input;"
                      " else echo 'No clipboard tool found' >&2; exit 1; fi")))
    (with-temp-buffer
      (insert content)
      (apply #'call-process-region (point-min) (point-max) "ssh" nil nil nil
             (append ssh-args (list remote-cmd))))
    (message "Pasted host clipboard -> VM (%d chars)" (length content))))

;;;###autoload
(defun qvm-ssh-copy-id (name)
  "Copy SSH public key to VM NAME for passwordless access.
Runs in a terminal buffer since it may prompt for a password."
  (interactive (list (completing-read "Push SSH key to VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (unless (qvm--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((conf (qvm--read-conf name))
         (user (qvm--conf-get conf "VM_USER"))
         (port (qvm--conf-get conf "VM_SSH_PORT"))
         (buf-name (format "*qvm ssh-copy-id: %s*" name)))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let* ((buf (make-term buf-name "ssh-copy-id" nil
                           "-p" port
                           "-o" "StrictHostKeyChecking=accept-new"
                           (format "%s@localhost" user)))
           (proc (get-buffer-process buf)))
      (with-current-buffer buf (term-mode) (term-char-mode))
      (when proc
        (set-process-sentinel
         proc
         (lambda (_proc event)
           (when (string-match-p "finished" event)
             (qvm--ssh-post-copy-id name)))))
      (pop-to-buffer buf))))

(defun qvm--ssh-post-copy-id (name)
  "Fix permissions and add SSH config after ssh-copy-id for VM NAME."
  (let* ((conf (qvm--read-conf name))
         (user (qvm--conf-get conf "VM_USER"))
         (port (qvm--conf-get conf "VM_SSH_PORT")))
    (call-process "ssh" nil nil nil
                  "-p" port
                  "-o" "StrictHostKeyChecking=accept-new"
                  (format "%s@localhost" user)
                  "chmod 700 ~/.ssh && chmod 600 ~/.ssh/authorized_keys")
    (if (= 0 (call-process "ssh" nil nil nil
                            "-p" port
                            "-o" "StrictHostKeyChecking=accept-new"
                            "-o" "BatchMode=yes"
                            (format "%s@localhost" user)
                            "true"))
        (progn
          (qvm--add-ssh-config name user port)
          (message "SSH key auth verified for '%s'" name))
      (message "Key auth verification failed for '%s'" name))))

;;;###autoload
(defun qvm-dired (name)
  "Open dired on VM NAME via TRAMP."
  (interactive (list (completing-read "Dired into VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (if (qvm--running-p name)
      (dired (qvm--tramp-path name))
    (user-error "VM '%s' is not running" name)))

;;;###autoload
(defun qvm-eshell (name)
  "Open an eshell on VM NAME via TRAMP."
  (interactive (list (completing-read "Shell into VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (if (qvm--running-p name)
      (let* ((default-directory (qvm--tramp-path name))
             (buf-name (format "*eshell: %s*" name)))
        (if (get-buffer buf-name)
            (pop-to-buffer buf-name)
          (let ((eshell-buffer-name buf-name))
            (eshell))))
    (user-error "VM '%s' is not running" name)))

;;;###autoload
(defun qvm-scp (name files remote-dir)
  "Copy FILES to VM NAME at REMOTE-DIR via rsync over SSH.
When called from a dired buffer, uses the marked files or the file at point.
Otherwise, prompts for a file."
  (interactive
   (let* ((vm (completing-read "SCP to VM: "
                               (seq-filter #'qvm--running-p (qvm--list-vms))
                               nil t))
          (files (if (derived-mode-p 'dired-mode)
                     (dired-get-marked-files nil nil nil nil t)
                   (list (read-file-name "File to send: " nil nil t))))
          (conf (qvm--read-conf vm))
          (user (qvm--conf-get conf "VM_USER"))
          (remote-dir (read-string "Remote directory: "
                                   (format "/home/%s/" user))))
     (list vm files remote-dir)))
  (unless (qvm--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((conf (qvm--read-conf name))
         (user (qvm--conf-get conf "VM_USER"))
         (port (qvm--conf-get conf "VM_SSH_PORT"))
         (ssh-cmd (if (qvm--ssh-config-host-p name)
                      "ssh"
                    (format "ssh -p %s" port)))
         (dest-host (if (qvm--ssh-config-host-p name)
                        (format "qvm-%s" name)
                      (format "%s@localhost" user)))
         (dest (format "%s:%s" dest-host remote-dir))
         (expanded (mapcar #'expand-file-name files))
         (args (append (list "-avz" "--progress" "-e" ssh-cmd)
                       expanded (list dest)))
         (buf (get-buffer-create qvm-output-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "$ rsync -avz --progress -e \"%s\" %s %s\n\n"
                      ssh-cmd
                      (mapconcat (lambda (f) (file-name-nondirectory f)) files " ")
                      dest))
      (setq buffer-read-only t))
    (display-buffer buf '(display-buffer-at-bottom . ((window-height . 10))))
    (make-process
     :name "qvm-scp"
     :buffer buf
     :command (cons "rsync" args)
     :filter (lambda (proc str)
               (with-current-buffer (process-buffer proc)
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert str))))
     :sentinel (lambda (proc event)
                 (with-current-buffer (process-buffer proc)
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert (format "\n[%s]" (string-trim event)))))))))

;;;###autoload
(defun qvm-snapshot-create (name tag)
  "Create a snapshot TAG for stopped VM NAME."
  (interactive
   (let* ((vms (seq-remove #'qvm--running-p (qvm--list-vms)))
          (name (completing-read "Snapshot VM: " vms nil t))
          (tag (read-string "Snapshot tag: ")))
     (list name tag)))
  (when (qvm--running-p name)
    (user-error "VM '%s' is running -- stop it first" name))
  (qvm--run-process "qvm-snapshot" "qemu-img"
                    (list "snapshot" "-c" tag (qvm--vm-disk name))))

;;;###autoload
(defun qvm-snapshot-list (name)
  "List snapshots for VM NAME."
  (interactive (list (completing-read "List snapshots for VM: " (qvm--list-vms) nil t)))
  (qvm--run-process "qvm-snapshot" "qemu-img"
                    (list "snapshot" "-l" (qvm--vm-disk name))))

;;;###autoload
(defun qvm-snapshot-restore (name tag)
  "Restore snapshot TAG for stopped VM NAME."
  (interactive
   (let* ((vms (seq-remove #'qvm--running-p (qvm--list-vms)))
          (name (completing-read "Restore snapshot for VM: " vms nil t))
          (tags (qvm--snapshot-tags name))
          (tag (completing-read "Snapshot to restore: " tags nil t)))
     (list name tag)))
  (when (qvm--running-p name)
    (user-error "VM '%s' is running -- stop it first" name))
  (when (yes-or-no-p (format "Restore snapshot '%s' for VM '%s'? " tag name))
    (qvm--run-process "qvm-snapshot" "qemu-img"
                      (list "snapshot" "-a" tag (qvm--vm-disk name)))))

;;;###autoload
(defun qvm-snapshot-delete (name tag)
  "Delete snapshot TAG from stopped VM NAME."
  (interactive
   (let* ((vms (seq-remove #'qvm--running-p (qvm--list-vms)))
          (name (completing-read "Delete snapshot from VM: " vms nil t))
          (tags (qvm--snapshot-tags name))
          (tag (completing-read "Snapshot to delete: " tags nil t)))
     (list name tag)))
  (when (qvm--running-p name)
    (user-error "VM '%s' is running -- stop it first" name))
  (when (yes-or-no-p (format "Delete snapshot '%s' from VM '%s'? " tag name))
    (qvm--run-process "qvm-snapshot" "qemu-img"
                      (list "snapshot" "-d" tag (qvm--vm-disk name)))))

;;;###autoload
(defun qvm-clone (name new-name linked)
  "Clone VM NAME to NEW-NAME.
With prefix argument or LINKED non-nil, create a linked (COW) clone."
  (interactive
   (let* ((name (completing-read "Clone VM: " (qvm--list-vms) nil t))
          (new-name (read-string "New VM name: "))
          (linked (yes-or-no-p "Create linked (COW) clone? ")))
     (list name new-name linked)))
  (when (qvm--running-p name)
    (user-error "VM '%s' is running -- stop it first" name))
  (when (string-empty-p new-name)
    (user-error "New VM name cannot be empty"))
  (when (file-exists-p (qvm--vm-conf new-name))
    (user-error "VM '%s' already exists" new-name))
  (let* ((conf (qvm--read-conf name))
         (ssh-port (qvm--next-free-port qvm-default-ssh-port))
         (vnc-display (qvm--next-free-vnc qvm-default-vnc-display))
         (spice-port (qvm--next-free-port qvm-default-spice-port))
         (new-conf (list (cons "VM_MEMORY" (or (qvm--conf-get conf "VM_MEMORY") qvm-default-memory))
                         (cons "VM_CPUS" (or (qvm--conf-get conf "VM_CPUS") qvm-default-cpus))
                         (cons "VM_DISK_SIZE" (or (qvm--conf-get conf "VM_DISK_SIZE") qvm-default-disk))
                         (cons "VM_SSH_PORT" (number-to-string ssh-port))
                         (cons "VM_VNC_DISPLAY" (number-to-string vnc-display))
                         (cons "VM_SPICE_PORT" (number-to-string spice-port))
                         (cons "VM_DISPLAY" (or (qvm--conf-get conf "VM_DISPLAY") qvm-default-display))
                         (cons "VM_USER" (or (qvm--conf-get conf "VM_USER") qvm-default-user)))))
    (make-directory (qvm--vm-dir new-name) t)
    (if linked
        (let ((src-disk (file-truename (qvm--vm-disk name))))
          (qvm--run-process
           "qvm-clone" "qemu-img"
           (list "create" "-f" "qcow2" "-b" src-disk "-F" "qcow2"
                 (qvm--vm-disk new-name))
           (lambda ()
             (qvm--write-conf new-name new-conf)
             (qvm-list-refresh))))
      (qvm--run-process
       "qvm-clone" "cp"
       (list (qvm--vm-disk name) (qvm--vm-disk new-name))
       (lambda ()
         (qvm--write-conf new-name new-conf)
         (qvm-list-refresh))))))

;;;###autoload
(defun qvm-info (name)
  "Show info for VM NAME."
  (interactive (list (completing-read "VM info: " (qvm--list-vms) nil t)))
  (let* ((conf (qvm--read-conf name))
         (running (qvm--running-p name))
         (memory (or (qvm--conf-get conf "VM_MEMORY") "?"))
         (cpus (or (qvm--conf-get conf "VM_CPUS") "?"))
         (disk-max (or (qvm--conf-get conf "VM_DISK_SIZE") "?"))
         (disk-used (qvm--disk-size name))
         (display (or (qvm--conf-get conf "VM_DISPLAY") "vnc"))
         (ssh-port (or (qvm--conf-get conf "VM_SSH_PORT") "?"))
         (user (or (qvm--conf-get conf "VM_USER") "?"))
         (vnc-display (qvm--conf-get conf "VM_VNC_DISPLAY"))
         (spice-port (qvm--conf-get conf "VM_SPICE_PORT")))
    (qvm--display-output
     (concat
      (format "%s\n" name)
      (format "  Status:    %s\n" (if running "running" "stopped"))
      (format "  Directory: %s\n" (qvm--vm-dir name))
      (format "  Disk:      %s (%s used / %s max)\n"
              (qvm--vm-disk name) disk-used disk-max)
      (format "  Memory:    %s\n" memory)
      (format "  CPUs:      %s\n" cpus)
      (format "  Display:   %s\n" display)
      (format "  SSH port:  %s\n" ssh-port)
      (if (string= display "spice")
          (format "  SPICE:     localhost:%s\n" (or spice-port "?"))
        (format "  VNC:       :%s (port %d)\n"
                (or vnc-display "?")
                (+ 5900 (string-to-number (or vnc-display "0")))))
      (format "  User:      %s\n" user)
      "\n"
      (format "  TRAMP:     %s\n" (qvm--tramp-path name))))))

;;;###autoload
(defun qvm-create (name iso disk memory cpus)
  "Create a new VM NAME, then boot ISO to install the OS.
ISO is taken from the file at point in a dired buffer, or prompted
via `read-file-name'.  DISK, MEMORY, and CPUS are prompted with defaults."
  (interactive
   (let* ((iso (if (and (derived-mode-p 'dired-mode)
                        (when-let ((f (dired-get-filename nil t)))
                          (string-match-p "\\.iso\\'" f)))
                   (dired-get-filename nil t)
                 (read-file-name "ISO image: " nil nil t nil
                                 (lambda (f)
                                   (or (file-directory-p f)
                                       (string-match-p "\\.iso\\'" f))))))
          (name   (read-string "VM name: "))
          (disk   (read-string "Disk size: " qvm-default-disk))
          (memory (read-string "Memory: " qvm-default-memory))
          (cpus   (read-string "CPUs: " qvm-default-cpus)))
     (list name iso disk memory cpus)))
  (when (file-exists-p (qvm--vm-conf name))
    (user-error "VM '%s' already exists" name))
  (let* ((ssh-port (qvm--next-free-port qvm-default-ssh-port))
         (vnc-display (qvm--next-free-vnc qvm-default-vnc-display))
         (spice-port (qvm--next-free-port qvm-default-spice-port))
         (conf (list (cons "VM_MEMORY" memory)
                     (cons "VM_CPUS" cpus)
                     (cons "VM_DISK_SIZE" disk)
                     (cons "VM_SSH_PORT" (number-to-string ssh-port))
                     (cons "VM_VNC_DISPLAY" (number-to-string vnc-display))
                     (cons "VM_SPICE_PORT" (number-to-string spice-port))
                     (cons "VM_DISPLAY" qvm-default-display)
                     (cons "VM_USER" qvm-default-user)))
         (expanded-iso (expand-file-name iso)))
    (make-directory (qvm--vm-dir name) t)
    (qvm--run-process
     "qvm-create" "qemu-img"
     (list "create" "-f" "qcow2" (qvm--vm-disk name) disk)
     (lambda ()
       (qvm--write-conf name conf)
       (let ((args (append (qvm--qemu-base-args name conf)
                           (list "-cdrom" expanded-iso "-boot" "d"))))
         (qvm--start-qemu name args (lambda () (qvm-list-refresh)))
         (qvm--append-output
          (format "\nInstalling from %s...\nQEMU running. After installation, stop the VM.\n"
                  (file-name-nondirectory expanded-iso)))
         (qvm-list-refresh))))))

;; ── Clip install (needs terminal for sudo) ───────────────────────────────────

(defun qvm--clip-install (name)
  "Install clipboard tools on VM NAME in a terminal buffer."
  (let* ((conf (qvm--read-conf name))
         (user (qvm--conf-get conf "VM_USER"))
         (port (qvm--conf-get conf "VM_SSH_PORT"))
         (buf-name (format "*qvm clip install: %s*" name))
         (remote-cmd (concat
                      "sudo pkill -9 PackageKit 2>/dev/null; sleep 1; "
                      "if command -v zypper >/dev/null 2>&1; then sudo zypper install -y wl-clipboard xclip; "
                      "elif command -v apt-get >/dev/null 2>&1; then sudo apt-get install -y wl-clipboard xclip; "
                      "elif command -v dnf >/dev/null 2>&1; then sudo dnf install -y wl-clipboard xclip; "
                      "elif command -v pacman >/dev/null 2>&1; then sudo pacman -S --noconfirm wl-clipboard xclip; "
                      "elif command -v apk >/dev/null 2>&1; then sudo apk add wl-clipboard xclip; "
                      "else echo 'Error: no supported package manager found' >&2; exit 1; fi")))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let ((buf (make-term buf-name "ssh" nil
                          "-t" "-p" port
                          "-o" "StrictHostKeyChecking=accept-new"
                          (format "%s@localhost" user)
                          remote-cmd)))
      (with-current-buffer buf (term-mode) (term-char-mode))
      (pop-to-buffer buf))))

;; ── List buffer ──────────────────────────────────────────────────────────────

(defvar qvm-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'qvm-list-run)
    (define-key map (kbd "r")   #'qvm-list-run)
    (define-key map (kbd "s")   #'qvm-list-start)
    (define-key map (kbd "x")   #'qvm-list-stop)
    (define-key map (kbd "c")   #'qvm-list-create)
    (define-key map (kbd "v")   #'qvm-list-vnc)
    (define-key map (kbd "V")   #'qvm-list-spice)
    (define-key map (kbd "d")   #'qvm-list-dired)
    (define-key map (kbd "e")   #'qvm-list-eshell)
    (define-key map (kbd "i")   #'qvm-list-info)
    (define-key map (kbd "D")   #'qvm-list-display)
    (define-key map (kbd "k")   #'qvm-list-keyboard)
    (define-key map (kbd "w")   #'qvm-list-clip-copy)
    (define-key map (kbd "y")   #'qvm-list-clip-paste)
    (define-key map (kbd "I")   #'qvm-list-clip-install)
    (define-key map (kbd "S")   #'qvm-list-scp)
    (define-key map (kbd "P")   #'qvm-list-ssh-copy-id)
    (define-key map (kbd "C")   #'qvm-list-clone)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "+")   #'qvm-list-snapshot-create)
    (define-key map (kbd "N")   #'qvm-list-snapshot-list)
    (define-key map (kbd "R")   #'qvm-list-snapshot-restore)
    (define-key map (kbd "X")   #'qvm-list-snapshot-delete)
    (define-key map (kbd "g")   #'qvm-list-refresh)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'qvm-menu)
    map)
  "Keymap for `qvm-list-mode'.")

(define-derived-mode qvm-list-mode tabulated-list-mode "QVM"
  "Major mode for the qvm VM list buffer."
  (setq tabulated-list-format
        [("Name"    20 t)
         ("Status"   8 t)
         ("Memory"   8 nil)
         ("CPUs"     5 nil)
         ("Disk"     8 nil)
         ("SSH"      6 nil)
         ("Display"  8 nil)
         ("Port"     6 nil)])
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header)
  (setq-local revert-buffer-function (lambda (&rest _) (qvm-list-refresh))))

(defun qvm--list-entries ()
  "Generate tabulated list entries for all VMs."
  (mapcar
   (lambda (name)
     (let* ((conf    (qvm--read-conf name))
            (running (qvm--running-p name))
            (status  (if running
                         (propertize "running" 'face '(:foreground "green"))
                       (propertize "stopped" 'face 'shadow)))
            (disk    (qvm--disk-size name))
            (memory  (or (qvm--conf-get conf "VM_MEMORY") "?"))
            (cpus    (or (qvm--conf-get conf "VM_CPUS") "?"))
            (ssh     (or (qvm--conf-get conf "VM_SSH_PORT") "?"))
            (display (or (qvm--conf-get conf "VM_DISPLAY") "vnc"))
            (port    (if (string= display "spice")
                         (or (qvm--conf-get conf "VM_SPICE_PORT") "?")
                       (or (qvm--conf-get conf "VM_VNC_DISPLAY") "?"))))
       (list name
             (vector
              (propertize name 'face 'bold)
              status memory cpus disk ssh display port))))
   (qvm--list-vms)))

(defun qvm-list-refresh ()
  "Refresh the VM list buffer."
  (interactive)
  (when-let ((buf (get-buffer "*qvm*")))
    (with-current-buffer buf
      (setq tabulated-list-entries (qvm--list-entries))
      (tabulated-list-print t)
      (qvm-list--goto-first-entry))))

(defun qvm-list--goto-first-entry ()
  "Move point to the first data row, skipping the header."
  (goto-char (point-min))
  (while (and (not (tabulated-list-get-id)) (not (eobp)))
    (forward-line 1)))

(defun qvm-list--current-name ()
  "Return the VM name at point in the list buffer."
  (or (tabulated-list-get-id)
      (user-error "Move point to a VM row first (use n/p or arrow keys)")))

(defun qvm-list-run ()
  "Run the VM at point (start + viewer)."
  (interactive)
  (qvm-run (qvm-list--current-name)))

(defun qvm-list-start ()
  "Start the VM at point."
  (interactive)
  (qvm-start (qvm-list--current-name)))

(defun qvm-list-stop ()
  "Stop the VM at point, with confirmation."
  (interactive)
  (let ((name (qvm-list--current-name)))
    (when (yes-or-no-p (format "Stop VM '%s'? " name))
      (qvm-stop name))))

(defun qvm-list-vnc ()
  "Open VNC viewer for VM at point."
  (interactive)
  (qvm-vnc (qvm-list--current-name)))

(defun qvm-list-spice ()
  "Open SPICE viewer for VM at point."
  (interactive)
  (qvm-spice (qvm-list--current-name)))

(defun qvm-list-dired ()
  "Open dired on the VM at point via TRAMP."
  (interactive)
  (qvm-dired (qvm-list--current-name)))

(defun qvm-list-eshell ()
  "Open eshell on the VM at point via TRAMP."
  (interactive)
  (qvm-eshell (qvm-list--current-name)))

(defun qvm-list-info ()
  "Show info for the VM at point."
  (interactive)
  (qvm-info (qvm-list--current-name)))

(defun qvm-list-display ()
  "Toggle display type (vnc/spice) for VM at point."
  (interactive)
  (qvm-display (qvm-list--current-name)))

(defun qvm-list-keyboard ()
  "Setup keyboard remaps and sticky keys on VM at point."
  (interactive)
  (qvm-keyboard (qvm-list--current-name)))

(defun qvm-list-clip-copy ()
  "Copy VM clipboard to host (w = kill/copy in Emacs)."
  (interactive)
  (qvm-clip-copy (qvm-list--current-name)))

(defun qvm-list-clip-paste ()
  "Paste host clipboard to VM (y = yank/paste in Emacs)."
  (interactive)
  (qvm-clip-paste (qvm-list--current-name)))

(defun qvm-list-clip-install ()
  "Install xclip on the VM at point via a terminal buffer."
  (interactive)
  (let ((name (qvm-list--current-name)))
    (when (yes-or-no-p (format "Install xclip on '%s'? " name))
      (qvm--clip-install name))))

(defun qvm-list-ssh-copy-id ()
  "Push SSH public key to the VM at point."
  (interactive)
  (qvm-ssh-copy-id (qvm-list--current-name)))

(defun qvm-list-scp ()
  "SCP files to the VM at point."
  (interactive)
  (let ((name (qvm-list--current-name)))
    (qvm-scp name
             (if (derived-mode-p 'dired-mode)
                 (dired-get-marked-files nil nil nil nil t)
               (list (read-file-name "File to send: " nil nil t)))
             (let* ((conf (qvm--read-conf name))
                    (user (qvm--conf-get conf "VM_USER")))
               (read-string "Remote directory: "
                            (format "/home/%s/" user))))))

(defun qvm-list-clone ()
  "Clone the VM at point."
  (interactive)
  (let ((name (qvm-list--current-name)))
    (qvm-clone name
               (read-string (format "Clone '%s' as: " name))
               (yes-or-no-p "Create linked (COW) clone? "))))

(defun qvm-list-snapshot-create ()
  "Create a snapshot for the VM at point."
  (interactive)
  (let ((name (qvm-list--current-name)))
    (qvm-snapshot-create name (read-string (format "Snapshot tag for '%s': " name)))))

(defun qvm-list-snapshot-list ()
  "List snapshots for the VM at point."
  (interactive)
  (qvm-snapshot-list (qvm-list--current-name)))

(defun qvm-list-snapshot-restore ()
  "Restore a snapshot for the VM at point."
  (interactive)
  (let* ((name (qvm-list--current-name))
         (tags (qvm--snapshot-tags name))
         (tag (completing-read (format "Restore snapshot for '%s': " name) tags nil t)))
    (qvm-snapshot-restore name tag)))

(defun qvm-list-snapshot-delete ()
  "Delete a snapshot from the VM at point."
  (interactive)
  (let* ((name (qvm-list--current-name))
         (tags (qvm--snapshot-tags name))
         (tag (completing-read (format "Delete snapshot from '%s': " name) tags nil t)))
    (qvm-snapshot-delete name tag)))

(defun qvm-list-create ()
  "Create a new VM from an ISO image."
  (interactive)
  (call-interactively #'qvm-create))

;;;###autoload
(defun qvm-list ()
  "Open the qvm VM manager buffer."
  (interactive)
  (let ((buf (get-buffer-create "*qvm*")))
    (with-current-buffer buf
      (qvm-list-mode)
      (setq tabulated-list-entries (qvm--list-entries))
      (tabulated-list-print)
      (qvm-list--goto-first-entry))
    (pop-to-buffer buf)))

;; ── Transient menu ───────────────────────────────────────────────────────────

;;;###autoload (autoload 'qvm-menu "qvm" nil t)
(transient-define-prefix qvm-menu ()
  "QVM - QEMU Virtual Machine Manager."
  [:if (lambda () (derived-mode-p 'qvm-list-mode))
   ["Lifecycle"
    ("s" "Start"              qvm-list-start)
    ("r" "Run (start+viewer)" qvm-list-run)
    ("x" "Stop"               qvm-list-stop)
    ("c" "Create new VM"      qvm-list-create)
    ("C" "Clone"              qvm-list-clone)]
   ["Connect"
    ("v" "VNC viewer"         qvm-list-vnc)
    ("V" "SPICE viewer"       qvm-list-spice)
    ("d" "Dired (TRAMP)"      qvm-list-dired)
    ("e" "Eshell (TRAMP)"     qvm-list-eshell)]]
  [:if (lambda () (derived-mode-p 'qvm-list-mode))
   ["Snapshots"
    ("+" "Create snapshot"    qvm-list-snapshot-create)
    ("N" "List snapshots"     qvm-list-snapshot-list)
    ("R" "Restore snapshot"   qvm-list-snapshot-restore)
    ("X" "Delete snapshot"    qvm-list-snapshot-delete)]
   ["Tools"
    ("S" "Send files (rsync)" qvm-list-scp)
    ("P" "Push SSH key"       qvm-list-ssh-copy-id)
    ("D" "Toggle display"     qvm-list-display)
    ("k" "Keyboard setup"     qvm-list-keyboard)
    ("w" "Clipboard copy"     qvm-list-clip-copy)
    ("y" "Clipboard paste"    qvm-list-clip-paste)
    ("I" "Install xclip"      qvm-list-clip-install)
    ("i" "VM info"            qvm-list-info)]]
  [:if-not (lambda () (derived-mode-p 'qvm-list-mode))
   :description "QVM"
   ("l" "Open VM list"       qvm-list)
   ("s" "Start VM"           qvm-start)
   ("x" "Stop VM"            qvm-stop)
   ("r" "Run VM"             qvm-run)
   ("c" "Create new VM"      qvm-create)
   ("i" "VM info"            qvm-info)])

(provide 'qvm)

;;; qvm.el ends here
