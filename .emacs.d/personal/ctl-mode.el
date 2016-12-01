(defun ctl-remedy-case ()
  "Opens a browser with the remedy case under the cursor.

If a bare number, it will preceed it with INC and pad with zeroes.
This command will open in the browser."

  (interactive)
  (let (inc)
    (setq inc
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
	    (current-word)))
    (browse-url
     (concat "https://remedy-itsm.savvis.net/arsys/forms/cmdbapppd/HPD:Help%20Desk/?qual=%271000000161%27=%22"
	     (ctl-gen-case-number inc) "%22"))
    (message
     (concat "Opened Incident " (ctl-gen-case-number inc) " in browser..."))))

(defun ctl-vmac ()
  "Opens a browser tab with the VMAC pulled up."
  (interactive)
  (let (vmac)
    (setq vmac
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (browse-url
     (concat "http://vantivereports.savvis.net/vantive/ActivityView.cgi?activityId="
	     vmac
	     "&SessionId=0&showImpacted=1&noteType=All&submit=submit"))))

(defun ctl-vmac-calendar ()
  "Opens a browser to the Unix T3 Calendar in Huge"
  (interactive)
  (browse-url "https://huge.it.savvis.net/vmac/calendar/workgroup/?workgroup=OCHC-VMAC-Unix&filter=Scheduled%3A+All"))
                        
(defun ctl-vmac-eww ()
  "Opens a browser tab with the VMAC pulled up."
  (interactive)
  (let (vmac)
    (setq vmac
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (eww
     (concat "http://vantivereports.savvis.net/vantive/ActivityView.cgi?activityId="
	     vmac
	     "&SessionId=0&showImpacted=1&noteType=All&submit=submit"))))

(defun ctl-gen-case-number (case-number)
  "Converts the passed argument to a Remedy case number in the format : INC############"
  (when
      (string-prefix-p "INC" case-number t)
    (setq case-number (substring case-number 3 nil)))
  (format "INC%012d" (string-to-number case-number)))

(defvar ctl-mode-map (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c 1") 'ctl-remedy-case)
	    (define-key map (kbd "C-c 2") 'ctl-ctl-vmac-calendar)
	    (define-key map (kbd "C-c 3") 'ctl-vmac)
	    (define-key map (kbd "C-c 4") 'ctl-vmac-eww)
	    map))

(define-minor-mode ctl-mode
  "Allow for shortcuts related to CTL functions (Remedy, Vantive, etc)"
  :lighter " CTL"
  :keymap 'ctl-mode-map )

(add-hook 'org-journal-mode-hook 'ctl-mode)
(add-hook 'lisp-interaction-mode-hook 'ctl-mode)

(provide 'ctl-mode)
