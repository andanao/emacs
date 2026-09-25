;;; linux.el --- Linux-only configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;; nixconfig
;;; Code:

(ads/leader-def "cn" "nix config" (projectile-switch-project-by-name "~/nix"))
(ads/leader-def "ch" "home-manager" (projectile-switch-project-by-name "~/home-manager"))

;;; linux.el ends here
