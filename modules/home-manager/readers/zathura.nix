{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.zathura;
in {
  options.custom.zathura = {
    enable = mkEnableOption "Enable support for zathura.";
  };

  config = mkIf cfg.enable {
    programs = {
      zathura = {
        enable = true;
        extraConfig = ''
          # ╭─────────────────────────────────────────────────────────────────────────────╮
          # │                                                                             │
          # │ File informations:                                                          │
          # │ - Name:    elems/zathurarc.tt                                               │
          # │ - Summary: The zathura configuration.                                       │
          # │ - Authors:                                                                  │
          # │   - Alessandro Molari <molari.alessandro@gmail.com> (alem0lars)             │
          # │                                                                             │
          # │ Project informations:                                                       │
          # │   - Homepage:        https://github.com/alem0lars/configs-zathura           │
          # │   - Getting started: see README.md in the project root folder               │
          # │                                                                             │
          # │ License: Apache v2.0 (see below)                                            │
          # │                                                                             │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          # │                                                                             │
          # │ Licensed to the Apache Software Foundation (ASF) under one more contributor │
          # │ license agreements.  See the NOTICE file distributed with this work for     │
          # │ additional information regarding copyright ownership. The ASF licenses this │
          # │ file to you under the Apache License, Version 2.0 (the "License"); you may  │
          # │ not use this file except in compliance with the License.                    │
          # │ You may obtain a copy of the License at                                     │
          # │                                                                             │
          # │   http://www.apache.org/licenses/LICENSE-2.0                                │
          # │                                                                             │
          # │ Unless required by applicable law or agreed to in writing, software         │
          # │ distributed under the License is distributed on an "AS IS# BASIS, WITHOUT   │
          # │ WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.            │
          # │ See the License for the specific language governing permissions and         │
          # │ limitations under the License.                                              │
          # │                                                                             │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          # ├────────────────────────────────────────────────────────────── Appearance ───┤
          # │ Show commandline, statusbar, horizontal and vertical scrollview.            │
          # ├────────────── Options ────────────── Value ───────────── Notes ─────────────┤
          set guioptions                         cshv                    #                │
          set font                               "monospace 16"          #                │
          set window-title-basename              "true"                  #                │
          set selection-clipboard                clipboard               #                │
          # ├─── (Base16 Spacemacs) ──────────────────────────────────────────── Theme ───┤
          # ├─── Author:Nasser Alshammari(https://github.com/nashamri/spacemacs-theme) ───┤
          # ├─────────────── Name ──────────────── Color ───────────── Notes ─────────────┤
          set default-bg                         "#292b2e"               #                │
          set default-fg                         "#b2b2b2"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set statusbar-bg                       "#5d4d7a"               #                │
          set statusbar-fg                       "#b2b2b2"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set inputbar-bg                        "#292b2e"               #                │
          set inputbar-fg                        "#b2b2b2"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set notification-bg                    "#292b2e"               #                │
          set notification-fg                    "#b2b2b2"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set notification-error-bg              "#292b2e"               #                │
          set notification-error-fg              "#f2241f"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set notification-warning-bg            "#292b2e"               #                │
          set notification-warning-fg            "#dc752f"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set highlight-color                    "#444155"               #                │
          set highlight-active-color             "#5d4d7a"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set completion-bg                      "#34323e"               #                │
          set completion-fg                      "#9a9aba"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set completion-highlight-bg            "#5e5079"               #                │
          set completion-highlight-fg            "#b2b2b2"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set completion-group-bg                "#c56ec3"               #                │
          set completion-group-fg                "#292b2e"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set index-bg                           "#34323e"               #                │
          set index-fg                           "#9a9aba"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set index-active-bg                    "#5e5079"               #                │
          set index-active-fg                    "#b2b2b2"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set recolor-lightcolor                 "#292b2e"               #                │
          set recolor-darkcolor                  "#e8e8e8"               #                │
          # ├─────────────────────────────────────────────────────────────────────────────┤
          set recolor                            "false"                 #                │
          set recolor-keephue                    "false"                 #                │
          # ├────────────────────────────────────────────────────────────────── Search ───┤
          set incremental-search                 "true"                  #                │
          set nohlsearch                         "false"                 #                │
          set jumplist-size                      2048                    #                │
          # ├───────────────────────────────────────────────────────────── Integration ───┤
          set synctex                            "true"                  #                │
          set synctex-editor-command             "code -g %{input}:%{line}" #             │
          set dbus-service                       "true"                  #                │
          # ├───────────────────────────────────────────────────────── File Navigation ───┤
          set show-directories                   "true"                  #                │
          # ╰─────────────────────────────────────────────────────────────────────────────╯
        '';
      };
    };
  };
}
