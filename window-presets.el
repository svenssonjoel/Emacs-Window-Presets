;;
;; Joel Svensson 2018
;;
;; Functions for restoring window layouts that I like 

;;
;; TODO: What happens if display-buffer-alist contains some other
;;       setting for shell. That other setting should, maybe, be deleted

;; ------------------------------------------------------------
;; Saving and restoring window layouts from a stack
;; ------------------------------------------------------------
(defvar window-layouts-stack ()
  "Contains stored window configurations")

(setq window-layouts-stack ())

(defun window-layout-push ()
  "Save current window layout to stack"
  (push (current-window-configuration) window-layouts-stack)
  )

(defun window-layout-pop ()
  "Restore window layout from top of the stack"
  (set-window-configuration (pop window-layouts-stack))
  )


;; ------------------------------------------------------------
;; Functions for recalling window layout presets
;; ------------------------------------------------------------

(defun reset-windows (l)
  "Removes all windows leaving just the one as the new root"
  (if ( > (length l) 1)
      (delete-window (car (reset-windows (cdr l))))
    ()
    )
  )

;; TODO: Maybe make this interactive? 
(defun window-preset (preset)
  "Apply a window preset"
  (configure-shell-open-in-active-window)
  (reset-windows (window-list))
  (funcall preset (car (window-list)))
  )
      

(defun lookup-list (s l)
  "Lookup string s in list l of (string val) pairs"
  (if l
      (let ((key (nth 0 (car l)))
	    (val (nth 1 (car l))))
	(if (string-match s key)
	    val
	  (lookup-list s (cdr l))
	  )
	)
    ()
    )
  )

(defun configure-shell-open-in-active-window ()
  "This function looks in the display-buffer-alist and"
  "adds display-buffer-same-window if not yet present for shell"
  (if (not ( eq
	     (lookup-list (regexp-quote "*shell") display-buffer-alist)
	     'display-buffer-same-window))
      (add-to-list 'display-buffer-alist
		   `(,(regexp-quote "*shell") display-buffer-same-window))
    ()
    )
  )


;; Example preset (presets are functions taking a root window as argument) 
(defun example-preset (root-window)
  "two editing windows and a shell console."
  "This function assumes there is only one window currently"
  (setq shell-window (split-window-below  (* 3 (/ (window-body-height) 4))))
  (split-window root-window () 'right)
  (with-selected-window shell-window (shell))
  )
