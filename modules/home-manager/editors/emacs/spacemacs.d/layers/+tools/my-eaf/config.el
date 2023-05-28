;;; config.el --- Emacs Application Framework Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defvar my-eaf-snails-enable nil
  "non-nil Snails enable.")

(defvar my-eaf-snails-enable-fuz nil
  "Snails will use fuzz match algorithm once you install fuz and add fuz in load-path.")

(spacemacs|defc eaf-apps
  '(eaf-airshare
    eaf-browser
    eaf-camera
    eaf-demo
    eaf-file-browser
    eaf-file-manager
    eaf-file-sender
    eaf-git
    eaf-image-viewer
    eaf-jupyter
    eaf-markdown-previewer
    eaf-mermaid
    eaf-mindmap
    eaf-music-player
    eaf-netease-cloud-music
    eaf-org-previewer
    eaf-pdf-viewer
    eaf-rss-reader
    eaf-system-monitor
    eaf-terminal
    eaf-video-player
    eaf-vue-demo)
  "The applications loaded from EAF package"
  '(set
    (const eaf-airshare)
    (const eaf-browser)
    (const eaf-camera)
    (const eaf-demo)
    (const eaf-file-browser)
    (const eaf-file-manager)
    (const eaf-file-sender)
    (const eaf-image-viewer)
    (const eaf-jupyter)
    (const eaf-markdown-previewer)
    (const eaf-mermaid)
    (const eaf-mindmap)
    (const eaf-music-player)
    (const eaf-netease-cloud-music)
    (const eaf-org-previewer)
    (const eaf-pdf-viewer)
    (const eaf-rss-reader)
    (const eaf-system-monitor)
    (const eaf-terminal)
    (const eaf-video-player)
    (const eaf-vue-demo))
  'eaf)


;;; config.el ends here
