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

;; Basics
(set-prefix-key (kbd "C-t"))

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

(setf *colors* (list "#073642"          ; 0 black
                     "#dc322f"          ; 1 red
                     "#859900"          ; 2 green
                     "#b58900"          ; 3 yellow
                     "#268bd2"          ; 4 blue
                     "#d33682"          ; 5 magenta
                     "#2aa198"          ; 6 cyan
                     "#eee8d5"))        ; 7 white

;;SOLARIZED HEX     16/8 TERMCOL  XTERM/HEX   L*A*B      RGB         HSB
;;--------- ------- ---- -------  ----------- ---------- ----------- -----------
;;base03    #002b36  8/4 brblack  234 #1c1c1c 15 -12 -12   0  43  54 193 100  21
;;base02    #073642  0/4 black    235 #262626 20 -12 -12   7  54  66 192  90  26
;;base01    #586e75 10/7 brgreen  240 #585858 45 -07 -07  88 110 117 194  25  46
;;base00    #657b83 11/7 bryellow 241 #626262 50 -07 -07 101 123 131 195  23  51
;;base0     #839496 12/6 brblue   244 #808080 60 -06 -03 131 148 150 186  13  59
;;base1     #93a1a1 14/4 brcyan   245 #8a8a8a 65 -05 -02 147 161 161 180   9  63
;;base2     #eee8d5  7/7 white    254 #e4e4e4 92 -00  10 238 232 213  44  11  93
;;base3     #fdf6e3 15/7 brwhite  230 #ffffd7 97  00  10 253 246 227  44  10  99
;;yellow    #b58900  3/3 yellow   136 #af8700 60  10  65 181 137   0  45 100  71
;;orange    #cb4b16  9/3 brred    166 #d75f00 50  50  55 203  75  22  18  89  80
;;red       #dc322f  1/1 red      160 #d70000 50  65  45 220  50  47   1  79  86
;;magenta   #d33682  5/5 magenta  125 #af005f 50  65 -05 211  54 130 331  74  83
;;violet    #6c71c4 13/5 brmagenta 61 #5f5faf 50  15 -45 108 113 196 237  45  77
;;blue      #268bd2  4/4 blue      33 #0087ff 55 -10 -45  38 139 210 205  82  82
;;cyan      #2aa198  6/6 cyan      37 #00afaf 60 -35 -05  42 161 152 175  74  63
;;green     #859900  2/2 green     64 #5f8700 60 -20  65 133 153   0  68 100  60

(set-fg-color (nth 0 *colors*))
(set-bg-color (nth 7 *colors*))
(set-border-color (nth 0 *colors*))
(set-focus-color "#586e75")
(set-unfocus-color "#073642")
(set-win-bg-color (nth 0 *colors*))
(set-float-focus-color "#586e75")
(set-float-unfocus-color "#073642")

(defun shift-windows-forward (frames win)
  "Exchange windows through cycling frames."
  (when frames
    (let ((frame (car frames)))
      (shift-windows-forward (cdr frames)
                             (stumpwm::frame-window frame))
      (when win
        (stumpwm::pull-window win frame)))))

(defcommand rotate-windows () ()
  (let* ((frames (stumpwm::head-frames (current-group) (current-head)))
         (win (stumpwm::frame-window (car (last frames)))))
    (shift-windows-forward frames win)))

(defun executable-find (name)
  "Tell if given executable is present in PATH."
  (let ((which-out (string-trim '(#\  #\linefeed) (run-shell-command (concat "which " name) t))))
    (unless (string-equal "" which-out) which-out)))

(defun slop-get-pos ()
  (mapcar #'parse-integer (ppcre:split "[^0-9]" (run-shell-command "slop -f \"%x %y %w %h\"" t))))

(defun slop-or-float ()
  "Slop the current window or just float if slop cli not present."
  (if (executable-find "slop")
      (let ((window (current-window))
            (pos (slop-get-pos)))
        (float-window window (current-group))
        (float-window-move-resize window
                                  :x (nth 0 pos)
                                  :y (nth 1 pos)
                                  :width (nth 2 pos)
                                  :height (nth 3 pos)))
      (run-commands "float-this")))

(defcommand slop-this () () (slop-or-float))

;; Define the background window
(defvar *background-image-path* "~/Pictures/Wallpapers/")
(defun select-random-background-image ()
  "Select a random image"
  (let ((file-list (directory (concatenate 'string *background-image-path* "*.jpg")))
        (*random-state* (make-random-state t)))
    (namestring (nth (random (length file-list)) file-list))))

(define-key *top-map* (kbd "s-r") "rotate-windows")
(define-key *top-map* (kbd "s-g") "toggle-gaps")
(define-key *top-map* (kbd "s-f") "slop-this")
(define-key *top-map* (kbd "s-u") "unfloat-this")
(define-key *top-map* (kbd "s-a") "toggle-always-show")
(define-key *top-map* (kbd "s-t") "toggle-always-on-top")

;; Init
(update-color-map (current-screen))
(run-shell-command "xrdb ~/.Xresources")
(run-shell-command "xmodmap .Xmodmap")
(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command (concatenate 'string "feh --bg-tile " (select-random-background-image)))
(run-shell-command "xset b off")
(run-shell-command "xscreensaver -nosplash")

;; Programs
(run-shell-command "picom")
