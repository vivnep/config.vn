(setq gc-cons-threshold 10000000) ; don't gc unless we're swapping
;; annoyance suppression
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message (user-login-name))
;; Default frame configuration: full screen, good-looking title bar on macOS
(tool-bar-mode -1) ;; needs to be here so fullscreen is set right
(setq frame-resize-pixelwise t)
(setq default-frame-alist '((fullscreen . maximized)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#808080")
                            (ns-transparent-titlebar . t)
			    ))
