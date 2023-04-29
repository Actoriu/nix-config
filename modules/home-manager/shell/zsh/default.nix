{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.zsh;
in {
  options.custom.zsh = {
    enable = mkEnableOption "Enable support for zsh.";
  };

  config = mkIf cfg.enable {
    programs = {
      zsh = {
        enable = cfg.enable;
        dotDir = ".config/zsh";
        history = {
          extended = true;
          # save = 100000;
          # size = 100000;
          path = "${config.xdg.dataHome}/zsh/zsh_history";
        };

        autocd = true;
        # enableAutosuggestions = true;

        initExtra = with pkgs; ''
          # setopt nomatch
          # setopt extendedglob
          # setopt rm_star_silent
          # setopt clobber
          # setopt combining_chars
          # setopt interactive_comments
          # setopt rc_quotes
          # unsetopt mail_warning
          # setopt long_list_jobs
          # setopt auto_resume
          # setopt notify
          # unsetopt bg_nice
          # unsetopt hup
          # unsetopt check_jobs
          # setopt auto_pushd
          # setopt pushd_ignore_dups
          # setopt pushd_silent
          # setopt pushd_to_home
          # setopt cdable_vars
          # setopt multios
          # setopt extended_glob
          # unsetopt clobber
          # setopt bang_hist
          # setopt complete_in_word
          # setopt always_to_end
          # setopt path_dirs
          # setopt auto_menu
          # setopt auto_list
          # setopt auto_param_slash
          # setopt extended_glob
          # unsetopt menu_complete
          # unsetopt flow_control
          # close bad pattern to use the character '#' for nix build
          unsetopt INTERACTIVE_COMMENTS
          unsetopt BAD_PATTERN

          export LESS_TERMCAP_mb=$'\E[01;31m'
          export LESS_TERMCAP_md=$'\E[01;31m'
          export LESS_TERMCAP_me=$'\E[0m'
          export LESS_TERMCAP_se=$'\E[0m'
          export LESS_TERMCAP_so=$'\E[00;47;30m'
          export LESS_TERMCAP_ue=$'\E[0m'
          export LESS_TERMCAP_us=$'\E[01;32m'

          [[ ! -f $XDG_CONFIG_HOME/zsh/.p10k.zsh ]] || source "$XDG_CONFIG_HOME/zsh/.p10k.zsh"
          # source ${zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
          # source ${zsh-fast-syntax-highlighting}/share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh
          # source ${zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh
          # source ${zsh-you-should-use}/share/zsh/plugins/you-should-use/you-should-use.plugin.zsh

          # eval "$(jump shell)"
          # eval $(thefuck --alias)
          function set_win_title() {
            echo -ne "\033]0; $TERM - $PWD \007"
                                     }
          precmd_functions+=(set_win_title)

          # Emacs
          bindkey -M emacs "^P" history-substring-search-up
          bindkey -M emacs "^N" history-substring-search-down

          # Vi
          bindkey -M vicmd "k" history-substring-search-up
          bindkey -M vicmd "j" history-substring-search-down

          # Emacs and Vi
          for keymap in 'emacs' 'viins'; do
            bindkey "$terminfo[kcuu1]" history-substring-search-up
            bindkey "$terminfo[kcud1]" history-substring-search-down
          done

          unset keymap
        '';

        plugins = [
          {
            name = "zsh-autosuggestions";
            src = pkgs.zsh-autosuggestions;
            file = "share/zsh-autosuggestions/zsh-autosuggestions.zsh";
          }
          {
            name = "zsh-fast-syntax-highlighting";
            src = pkgs.zsh-fast-syntax-highlighting;
            file = "share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh";
          }
          {
            name = "zsh-history-substring-search";
            src = pkgs.zsh-history-substring-search;
            file = "share/zsh-history-substring-search/zsh-history-substring-search.zsh";
          }
          {
            name = "powerlevel10k";
            src = pkgs.zsh-powerlevel10k;
            file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
          }
          # {
          #   name = "powerlevel10k-config";
          #   src = pkgs.substituteAll { src=./zsh-p10k.zsh; dir="bin"; };
          #   file = "bin/zsh-p10k.zsh";
          # }
        ];
      };
    };
  };
}
