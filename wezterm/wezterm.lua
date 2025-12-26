local wezterm = require 'wezterm'

local config = {}

-- Font and appearance
config.font = wezterm.font("JetBrainsMono Nerd Font")
config.font_size = 11.0
config.color_scheme = "Dracula"

config.window_background_opacity = 0.95
config.window_padding = {
  left = 6,
  right = 6,
  top = 6,
  bottom = 6,
}

config.adjust_window_size_when_changing_font_size = false
config.window_decorations = "RESIZE"

config.tab_bar_at_bottom = false
config.hide_tab_bar_if_only_one_tab = false

-- Plugin setup
local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")

tabline.setup({
  options = {
    icons_enabled = true,
    theme = "Dracula",

    -- Thick half-circle separators
    section_separators = {
      left  = wezterm.nerdfonts.ple_right_half_circle_thick,
      right = wezterm.nerdfonts.ple_left_half_circle_thick,
    },
    component_separators = {
      left  = wezterm.nerdfonts.ple_right_half_circle_thick,
      right = wezterm.nerdfonts.ple_left_half_circle_thick,
    },
    tab_separators = {
      left  = wezterm.nerdfonts.ple_right_half_circle_thick,
      right = wezterm.nerdfonts.ple_left_half_circle_thick,
    },
  },

  -- Sections
  sections = {
    tabline_a = { "mode" },
    tabline_b = {},
    tabline_c = { " " },

    tab_active = {
      "index",
      { "cwd", padding = { left = 1, right = 1 } },
      { "process", padding = { left = 1, right = 1 } },
    },

    tab_inactive = {
      { "index", padding = { left = 1, right = 1 } },
      { "process", padding = { left = 1, right = 1 } },
    },

    -- Only show hostname on the right
    tabline_x = {},          -- remove datetime
    tabline_y = {},          -- remove RAM/CPU
    tabline_z = { "domain" }, -- hostname
  },

  extensions = {},
})

-- Apply plugin to config
tabline.apply_to_config(config)

return config
