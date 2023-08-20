;;; funcs.el --- Scad Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defun scad-open-stl-with-blender ()
  "Open the associated stl file with blender"
  (interactive)
  (let* ((filename (buffer-file-name))
         (stlname (format "%s.stl" (file-name-sans-extension filename)))
         (arg (format "import bpy; bpy.ops.import_mesh.stl(filepath='%s')" stlname)))
    (if (executable-find "blender")
        (start-process "blender" nil "blender" "--python-expr" arg)
      (message "Is not install blender."))))

(defun scad-preview-show ()
  "Scad preview show."
  (interactive)
  (setq scad-preview-image-size  `( ,(/ (frame-pixel-width) 2)
                                    .
                                    ,(frame-pixel-height))
        scad-preview-window-size (/ (frame-width) 2))
  (let ((scad-buff-name "*SCAD Preview*"))
    (delete-other-windows)
    (if scad-preview-mode
        (if (bufferp (get-buffer scad-buff-name))
            (display-buffer scad-buff-name 'display-buffer-pop-up-window)
          (scad-preview-mode)
          (scad-preview-mode))
      (scad-preview-mode)))
  (scad-preview-refresh))

(defun scad-preview-quit ()
  "Scad preview quit."
  (interactive)
  (let ((scad-buff-name "*SCAD Preview*"))
    (when (bufferp (get-buffer scad-buff-name))
      (kill-buffer (get-buffer scad-buff-name)))
    (when scad-preview-mode
      (scad-preview-mode))))


;;; funcs.el ends here
