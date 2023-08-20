;;; packages.el --- C-C++ Layer packages File for Spacemacs
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

(defconst my-c-c++-packages
  '(
    ccls
    )
  )


(defun my-c-c++/post-init-ccls ()
  (cond ((spacemacs/system-is-linux)
         (with-eval-after-load 'ccls
           (setq ccls-initialization-options
                 `(:clang
                   (:excludeArgs
                    ;; Linux's gcc options. See ccls/wiki
                    ["-falign-jumps=1" "-falign-loops=1" "-fconserve-stack" "-fmerge-constants" "-fno-code-hoisting" "-fno-schedule-insns" "-fno-var-tracking-assignments" "-fsched-pressure"
                     "-mhard-float" "-mindirect-branch-register" "-mindirect-branch=thunk-inline" "-mpreferred-stack-boundary=2" "-mpreferred-stack-boundary=3" "-mpreferred-stack-boundary=4" "-mrecord-mcount" "-mindirect-branch=thunk-extern" "-mno-fp-ret-in-387" "-mskip-rax-setup"
                     "--param=allow-store-data-races=0" "-Wa arch/x86/kernel/macros.s" "-Wa -"]
                    :extraArgs ["--gcc-toolchain=/usr"]
                    :pathMappings [])
                   :completion
                   (:include
                    (:blacklist
                     ["^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
                      "^/usr/(local/)?include/c\\+\\+/v1/"
                      ]))
                   :index (:initialBlacklist [] :parametersInDeclarations :json-false :trackDependency 1)))
           ))
        ((spacemacs/system-is-mac)
         (with-eval-after-load 'ccls
           (setq ccls-initialization-options
                 `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                             "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                             "-isystem/usr/local/include"]
                                 :resourceDir (string-trim (shell-command-to-string "clang -print-resource-dir"))))))
         ))
  )


;;; packages.el ends here
