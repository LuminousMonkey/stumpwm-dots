(in-package :stumpwm)

(require :swank)
(swank-loader:init)

(swank:create-server
 :port 4444
 :style swank:*communication-style*
 :dont-close t)

(setf *startup-message* NIL
      *suppress-abort-messages* t
      *shell-program* (getenv "SHELL"))

(set-module-dir
 (pathname-as-directory (concat (getenv "HOME")
                                "/.stumpwm.d/stumpwm-contrib")))

;; Theming
(setf *window-border-style* :none)
(load-module "ttf-fonts")
(clx-truetype:cache-fonts)
(set-font (make-instance 'xft:font
       :family "Fira Code Medium"
       :subfamily "Regular"
       :size 12
       :antialias t))

;; Gaps
(load-module "swm-gaps")
(load-module "kbd-layouts")

(setf kbd-layouts:*caps-lock-behavior* :ctrl)

(setf swm-gaps:*inner-gaps-size* 10
      swm-gaps:*inner-gaps-size* 10
      swm-gaps:*head-gaps-size* 10)

(run-commands "toggle-gaps")

(setf *message-window-gravity* :center
      *input-window-gravity* :center
      *window-border-style* :none
      *message-window-padding* 10
      *maxsize-border-width* 0
      *normal-border-width* 5
      *transient-border-width* 2
      stumpwm::*float-window-border* 2
      stumpwm::*float-window-title-height* 5
      *mouse-focus-policy* :click)

(set-normal-gravity :center)
(set-maxsize-gravity :center)
(set-transient-gravity :center)

(defun window-cls-present-p (win-cls &optional all-groups)
  "Tell if a window (by class) is present"
  (let ((windows (group-windows (if all-groups (current-screen) (current-group)))))
    (member win-cls (mapcar #'window-class windows) :test #'string-equal)))

(defun run-or-raise-prefer-group (cmd win-cls)
  "If there are windows in the same class, cycle in those. Otherwise call
run-or-raise with group search t."
  (if (window-cls-present-p win-cls)
      (run-or-raise cmd `(:class ,win-cls) nil T)
      (run-or-raise cmd `(:class ,win-cls) T T)))

(defcommand emacs () ()
  (run-or-raise-prefer-group "ec" "Emacs"))

;; Keybindings
;; motion
(define-key *root-map* (kbd "h") "move-focus left")
(define-key *root-map* (kbd "l") "move-focus right")
(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "j") "move-focus down")

(define-key *root-map* (kbd "e") "exec ec")
(define-key *root-map* (kbd "c") "exec kitty")

(set-fg-color "#eee8d5")
(set-bg-color "#586e75")
(set-border-color "#fdf6e3")

;; Init
(update-color-map (current-screen))
(run-shell-command "sh ~/.fehbg")
(run-shell-command "xset b off")

;; Programs
(run-shell-command "picom")
