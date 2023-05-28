{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.alacritty;
in {
  options.custom.alacritty = {
    enable = mkEnableOption "Enable support for alacritty.";
  };

  config = mkIf cfg.enable {
    programs = {
      alacritty = {
        enable = cfg.enable;
        settings = {
          env = {
            TERM = "xterm-256color";
          };

          window = {
            dynamic_padding = true;
            dynamic_title = true;
          };

          scrolling = {
            history = 65536;
            multiplier = 3;
          };

          font = {
            normal = {
              family = "monospace";
              style = "Regular";
            };

            bold = {
              family = "monospace";
              style = "Bold";
            };

            italic = {
              family = "monospace";
              style = "Italic";
            };

            bold_italic = {
              family = "monospace";
              style = "Bold Italic";
            };

            size = 16.0;
          };

          draw_bold_text_with_bright_colors = true;

          # colors = {
          #   primary = {
          #     background = "#2e3440";
          #     foreground = "#d8dee9";

          #     dim_foreground = "#a5abb6";
          #   };

          #   cursor = {
          #     text = "#2e3440";
          #     cursor = "#d8dee9";
          #   };

          #   vi_mode_cursor = {
          #     text = "#2e3440";
          #     cursor = "#d8dee9";
          #   };

          #   selection = {
          #     text = "CellForeground";
          #     background = "#4c566a";
          #   };

          #   search = {
          #     matches = {
          #       foreground = "CellBackground";
          #       background = "#88c0d0";
          #     };
          #     bar = {
          #       background = "#434c5e";
          #       foreground = "#d8dee9";
          #     };
          #   };

          #   normal = {
          #     black = "#3b4252";
          #     red = "#bf616a";
          #     green = "#a3be8c";
          #     yellow = "#ebcb8b";
          #     blue = "#81a1c1";
          #     magenta = "#b48ead";
          #     cyan = "#88c0d0";
          #     white = "#e5e9f0";
          #   };

          #   bright = {
          #     black = "#4c566a";
          #     red = "#bf616a";
          #     green = "#a3be8c";
          #     yellow = "#ebcb8b";
          #     blue = "#81a1c1";
          #     magenta = "#b48ead";
          #     cyan = "#8fbcbb";
          #     white = "#eceff4";
          #   };

          #   dim = {
          #     black = "#373e4d";
          #     red = "#94545d";
          #     green = "#809575";
          #     yellow = "#b29e75";
          #     blue = "#68809a";
          #     magenta = "#8c738c";
          #     cyan = "#6d96a5";
          #     white = "#aeb3bb";
          #   };
          # };

          live_config_reload = true;

          # https://github.com/rummik/nixos-config/blob/master/config/home-manager/alacritty.nix#L565
          key_bindings = flatten [
            # { key = "Paste";                                             action = "Paste";                        }
            # { key = "Copy";                                              action = "Copy";                         }
            # { key = "L";         mods = "Control";                       action = "ClearLogNotice";               }
            # { key = "L";         mods = "Control"; mode = "~Vi|~Search"; chars = "\\x0c";                         }
            # { key = "PageUp";    mods = "Shift";   mode = "~Alt";        action = "ScrollPageUp";                 }
            # { key = "PageDown";  mods = "Shift";   mode = "~Alt";        action = "ScrollPageDown";               }
            # { key = "Home";      mods = "Shift";   mode = "~Alt";        action = "ScrollToTop";                  }
            # { key = "End";       mods = "Shift";   mode = "~Alt";        action = "ScrollToBottom";               }

            # Vi Mode
            # { key = "Space";  mods = "Shift|Control"; mode = "~Search";    action = "ToggleViMode";               }
            # { key = "Space";  mods = "Shift|Control"; mode = "Vi|~Search"; action = "ScrollToBottom";             }
            # { key = "Escape";                         mode = "Vi|~Search"; action = "ClearSelection";             }
            # { key = "I";                              mode = "Vi|~Search"; action = "ToggleViMode";               }
            # { key = "I";                              mode = "Vi|~Search"; action = "ScrollToBottom";             }
            # { key = "C";      mods = "Control";       mode = "Vi|~Search"; action = "ToggleViMode";               }
            # { key = "Y";      mods = "Control";       mode = "Vi|~Search"; action = "ScrollLineUp";               }
            # { key = "E";      mods = "Control";       mode = "Vi|~Search"; action = "ScrollLineDown";             }
            # { key = "G";                              mode = "Vi|~Search"; action = "ScrollToTop";                }
            # { key = "G";      mods = "Shift";         mode = "Vi|~Search"; action = "ScrollToBottom";             }
            # { key = "B";      mods = "Control";       mode = "Vi|~Search"; action = "ScrollPageUp";               }
            # { key = "F";      mods = "Control";       mode = "Vi|~Search"; action = "ScrollPageDown";             }
            # { key = "U";      mods = "Control";       mode = "Vi|~Search"; action = "ScrollHalfPageUp";           }
            # { key = "D";      mods = "Control";       mode = "Vi|~Search"; action = "ScrollHalfPageDown";         }
            # { key = "Y";                              mode = "Vi|~Search"; action = "Copy";                       }
            # { key = "Y";                              mode = "Vi|~Search"; action = "ClearSelection";             }
            # { key = "Copy";                           mode = "Vi|~Search"; action = "ClearSelection";             }
            # { key = "V";                              mode = "Vi|~Search"; action = "ToggleNormalSelection";      }
            # { key = "V";      mods = "Shift";         mode = "Vi|~Search"; action = "ToggleLineSelection";        }
            # { key = "V";      mods = "Control";       mode = "Vi|~Search"; action = "ToggleBlockSelection";       }
            # { key = "V";      mods = "Alt";           mode = "Vi|~Search"; action = "ToggleSemanticSelection";    }
            # { key = "Return";                         mode = "Vi|~Search"; action = "Open";                       }
            # { key = "K";                              mode = "Vi|~Search"; action = "Up";                         }
            # { key = "J";                              mode = "Vi|~Search"; action = "Down";                       }
            # { key = "H";                              mode = "Vi|~Search"; action = "Left";                       }
            # { key = "L";                              mode = "Vi|~Search"; action = "Right";                      }
            # { key = "Up";                             mode = "Vi|~Search"; action = "Up";                         }
            # { key = "Down";                           mode = "Vi|~Search"; action = "Down";                       }
            # { key = "Left";                           mode = "Vi|~Search"; action = "Left";                       }
            # { key = "Right";                          mode = "Vi|~Search"; action = "Right";                      }
            # { key = "Key0";                           mode = "Vi|~Search"; action = "First";                      }
            # { key = "Key4";   mods = "Shift";         mode = "Vi|~Search"; action = "Last";                       }
            # { key = "Key6";   mods = "Shift";         mode = "Vi|~Search"; action = "FirstOccupied";              }
            # { key = "H";      mods = "Shift";         mode = "Vi|~Search"; action = "High";                       }
            # { key = "M";      mods = "Shift";         mode = "Vi|~Search"; action = "Middle";                     }
            # { key = "L";      mods = "Shift";         mode = "Vi|~Search"; action = "Low";                        }
            # { key = "B";                              mode = "Vi|~Search"; action = "SemanticLeft";               }
            # { key = "W";                              mode = "Vi|~Search"; action = "SemanticRight";              }
            # { key = "E";                              mode = "Vi|~Search"; action = "SemanticRightEnd";           }
            # { key = "B";      mods = "Shift";         mode = "Vi|~Search"; action = "WordLeft";                   }
            # { key = "W";      mods = "Shift";         mode = "Vi|~Search"; action = "WordRight";                  }
            # { key = "E";      mods = "Shift";         mode = "Vi|~Search"; action = "WordRightEnd";               }
            # { key = "Key5";   mods = "Shift";         mode = "Vi|~Search"; action = "Bracket";                    }
            # { key = "Slash";                          mode = "Vi|~Search"; action = "SearchForward";              }
            # { key = "Slash";  mods = "Shift";         mode = "Vi|~Search"; action = "SearchBackward";             }
            # { key = "N";                              mode = "Vi|~Search"; action = "SearchNext";                 }
            # { key = "N";      mods = "Shift";         mode = "Vi|~Search"; action = "SearchPrevious";             }

            # Search Mode
            # { key = "Return";                   mode = "Search|Vi";  action = "SearchConfirm";                    }
            # { key = "Escape";                   mode = "Search";     action = "SearchCancel";                     }
            # { key = "C";      mods = "Control"; mode = "Search";     action = "SearchCancel";                     }
            # { key = "U";      mods = "Control"; mode = "Search";     action = "SearchClear";                      }
            # { key = "W";      mods = "Control"; mode = "Search";     action = "SearchDeleteWord";                 }
            # { key = "P";      mods = "Control"; mode = "Search";     action = "SearchHistoryPrevious";            }
            # { key = "N";      mods = "Control"; mode = "Search";     action = "SearchHistoryNext";                }
            # { key = "Up";                       mode = "Search";     action = "SearchHistoryPrevious";            }
            # { key = "Down";                     mode = "Search";     action = "SearchHistoryNext";                }
            # { key = "Return";                   mode = "Search|~Vi"; action = "SearchFocusNext";                  }
            # { key = "Return"; mods = "Shift";   mode = "Search|~Vi"; action = "SearchFocusPrevious";              }

            # (Windows, Linux, and BSD only)
            # (optionals (pkgs.stdenv.isLinux || pkgs.stdenv.isWindows) [
            #   { key = "V";              mods = "Control|Shift"; mode = "~Vi";        action = "Paste";            }
            #   { key = "C";              mods = "Control|Shift";                      action = "Copy";             }
            #   { key = "F";              mods = "Control|Shift"; mode = "~Search";    action = "SearchForward";    }
            #   { key = "B";              mods = "Control|Shift"; mode = "~Search";    action = "SearchBackward";   }
            #   { key = "C";              mods = "Control|Shift"; mode = "Vi|~Search"; action = "ClearSelection";   }
            #   { key = "Insert";         mods = "Shift";                              action = "PasteSelection";   }
            #   { key = "Key0";           mods = "Control";                            action = "ResetFontSize";    }
            #   { key = "Equals";         mods = "Control";                            action = "IncreaseFontSize"; }
            #   { key = "Plus";           mods = "Control";                            action = "IncreaseFontSize"; }
            #   { key = "NumpadAdd";      mods = "Control";                            action = "IncreaseFontSize"; }
            #   { key = "Minus";          mods = "Control";                            action = "DecreaseFontSize"; }
            #   { key = "NumpadSubtract"; mods = "Control";                            action = "DecreaseFontSize"; }
            # ])

            # (Windows only)
            # (optionals pkgs.stdenv.isWindows [
            #   { key = "Return"; mods = "Alt"; action = "ToggleFullscreen";                                        }
            # ])

            # (macOS only)
            # (optionals pkgs.stdenv.isDarwin [
            #   { key = "K";              mods = "Command"; mode = "~Vi|~Search"; chars = "\\x0c";                  }
            #   { key = "K";              mods = "Command"; mode = "~Vi|~Search"; action = "ClearHistory";          }
            #   { key = "Key0";           mods = "Command";                       action = "ResetFontSize";         }
            #   { key = "Equals";         mods = "Command";                       action = "IncreaseFontSize";      }
            #   { key = "Plus";           mods = "Command";                       action = "IncreaseFontSize";      }
            #   { key = "NumpadAdd";      mods = "Command";                       action = "IncreaseFontSize";      }
            #   { key = "Minus";          mods = "Command";                       action = "DecreaseFontSize";      }
            #   { key = "NumpadSubtract"; mods = "Command";                       action = "DecreaseFontSize";      }
            #   { key = "V";              mods = "Command";                       action = "Paste";                 }
            #   { key = "C";              mods = "Command";                       action = "Copy";                  }
            #   { key = "C";              mods = "Command"; mode = "Vi|~Search";  action = "ClearSelection";        }
            #   { key = "H";              mods = "Command";                       action = "Hide";                  }
            #   { key = "H";              mods = "Command|Alt";                   action = "HideOtherApplications"; }
            #   { key = "M";              mods = "Command";                       action = "Minimize";              }
            #   { key = "Q";              mods = "Command";                       action = "Quit";                  }
            #   { key = "W";              mods = "Command";                       action = "Quit";                  }
            #   { key = "N";              mods = "Command";                       action = "SpawnNewInstance";      }
            #   { key = "F";              mods = "Command|Control";               action = "ToggleFullscreen";      }
            #   { key = "F";              mods = "Command"; mode = "~Search";     action = "SearchForward";         }
            #   { key = "B";              mods = "Command"; mode = "~Search";     action = "SearchBackward";        }
            # ])
          ];
        };
      };
    };
  };
}
