;; (setq gc-cons-percentage 0.6)
;; (setq comp-async-report-warnings-errors nil)
;; (setq idle-update-delay 1.0)
;; (setq-default bidi-display-reordering 'left-to-right 
;;               bidi-paragraph-direction 'left-to-right)
;; (setq bidi-inhibit-bpa t)

;; copied from doom:
(setq gc-cons-threshold most-positive-fixnum)
(setq native-comp-deferred-compilation nil)
(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)
(set-language-environment "UTF-8")
(setq default-input-method nil)
