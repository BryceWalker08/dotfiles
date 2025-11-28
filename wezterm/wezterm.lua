local wezterm = require ("wezterm")
return {
    font = wezterm.font("JetBrainsMono Nerd Font"),
    font_size = 11.0,

    color_scheme = "Dracula",
    enable_tab_bar = false,

    window_background_opacity = 0.95,
    window_padding = {
      left = 6,
      right = 6,
      top = 6,
      bottom = 6,
    },

    adjust_window_size_when_changing_font_size = false,
    window_decorations = "RESIZE",
}
